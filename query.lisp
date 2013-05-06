(in-package :cl-rethinkdb)

(define-condition query-error (simple-error)
  ((token :reader query-error-token :initarg :token :initform nil)
   (msg :reader query-error-msg :initarg :msg :initform ""))
  (:report (lambda (c s) (format s "Query failed (~a): ~a" (query-error-token c) (query-error-msg c))))
  (:documentation "A general query failure condition."))

(define-condition query-client-error (query-error) ()
  (:report (lambda (c s) (format s "Client error: Query (~a): ~a" (query-error-token c) (query-error-msg c))))
  (:documentation "A client error condition."))

(define-condition query-compile-error (query-error) ()
  (:report (lambda (c s) (format s "Query failed to compile (~a): ~a" (query-error-token c) (query-error-msg c))))
  (:documentation "A query compile error condition."))
  
(define-condition query-runtime-error (query-error) ()
  (:report (lambda (c s) (format s "Query runtime error (~a): ~a" (query-error-token c) (query-error-msg c))))
  (:documentation "A query runtime error condition."))

(define-condition cursor-error (simple-error)
  ((token :reader cursor-error-token :initarg :token :initform nil)
   (cursor :reader cursor-error-cursor :initarg :cursor :initform nil))
  (:report (lambda (c s) (format s "Cursor error (~a): ~a" (cursor-error-token c) (cursor-error-cursor c))))
  (:documentation "Describes a general query error."))

(define-condition cursor-no-more-results (cursor-error) ()
  (:report (lambda (c s) (format s "No more results on cursor (~a): ~a" (cursor-error-token c) (cursor-error-cursor c))))
  (:documentation "Thrown when a cursor has no more results on it."))

(define-condition cursor-overshot (cursor-error) ()
  (:report (lambda (c s) (format s "Cursor overshot, don't call `next` without waiting for results (~a): ~a"
                                 (cursor-error-token c)
                                 (cursor-error-cursor c))))
  (:documentation "Thrown when a cursor has no more results on it."))

(defclass state ()
  ((token :accessor state-token :initform 0)
   (active-queries :accessor active-queries :initform (make-hash-table :test #'eq)))
  (:documentation "Tracks all state for the driver."))

(defmethod print-object ((state state) s)
  (print-unreadable-object (state s :type t :identity t)
    (format s "~_token: ~s " (state-token state))
    (format s "~_queries: ~s" (hash-table-count (active-queries state)))))

(defclass cursor ()
  ((state :accessor cursor-state :initarg :state :initform :new
     :documentation "Describes the current state of the query (new, complete, etc).")
   (future :accessor cursor-future :initarg :future :initform nil
     :documentation "Holds the future that will be finished with the results from this query.")
   (token :accessor cursor-token :initarg :token :initform nil
     :documentation "Holds the token for this query.")
   (last-change :accessor cursor-last-change :initform 0
     :documentation "Tracks the last time the query changed state.")
   (results :accessor cursor-results :initform nil
     :documentation "Holds the current result set from the query.")
   (current-result :accessor cursor-current-result :initform 0
     :documentation "Tracks which record the cursor points to."))
  (:documentation
    "The query class holds the state of a query, as well as the future that will
     be finished when the query returns results."))

(defmethod (setf cursor-state) :after (x (cursor cursor))
  "Track whenever the state changes in a query."
  (declare (ignore x))
  (setf (cursor-last-change cursor) (get-internal-real-time)))

(defmethod (setf cursor-results) :after (x (cursor cursor))
  "Make sure to reset the curr-result pointer when setting in new results."
  (declare (ignore x))
  (setf (cursor-current-result cursor) 0))

(defun cursorp (cursor)
  "Determine if the given object is a cursor."
  (typep cursor 'cursor))

(defclass connection-options ()
  ((kv :accessor conn-kv :initarg :kv :initform nil))
  (:documentation "Holds per-connection options."))

(defvar *state* (make-instance 'state)
  "Holds all tracking state for the RethinkDB driver.")

(defun generate-token (&key (state *state*))
  "Generates a new token value for a query."
  (prog1 (state-token state) (incf (state-token state))))

(defun save-cursor (token cursor &key (state *state*))
  "Associate a cursor with a token. Retrievable through get-cursor."
  (setf (gethash token (active-queries state)) cursor))

(defun get-cursor (token &key (state *state*))
  "Grab a cursor associated with a token. Returns nil if no cursor exists for that
   token"
  (gethash token (active-queries state)))

(defun remove-cursor (cursor &key (state *state*))
  "Remove a cursor/token from state tracking."
  (let ((token (cursor-token cursor)))
    (remhash token (active-queries state))))

(defun make-response-handler ()
  "This function returns a closure that can be called multiple times with data
   from a RethinkDB response. If a full response is received (over one or more
   calls) it returns the *full byte array of the response*, otherwise nil.
   
   Note that the response chunks MUST be passed in in the order received."
  (let ((response-bytes (make-array 0 :element-type '(unsigned-byte 8)))
        (response-size (the fixnum 0)))
    (lambda (bytes)
      ;; if we don't have a response size, grab it so we know when to return
      (when (zerop response-size)
        ;; make sure we at least have the response size integer
        (when (<= 4 (length bytes))
          ;; convert the little-endian response size into an integer, stored in
          ;; response-size
          (dotimes (i 4)
            (setf (ldb (byte 8 (* i 8)) response-size) (aref bytes i)))
          ;; trim the response length (it's not part of the protobuf)
          (setf bytes (subseq bytes 4))))
      (setf response-bytes (cl-async-util:append-array response-bytes bytes))
      (when (<= response-size (length response-bytes))
        response-bytes))))

(defun parse-response (response)
  "Parses a RethinkDB response (deserialized into protobuf classes) into a
   lispy/readable format.
   
   Also throws any errors encountered in the response (client error, compile
   error, runtime error)."
  (let* ((response-type (rdp:type response))
         (token (rdp:token response))
         (cursor (get-cursor token))
         (future (cursor-future cursor))
         (value nil)
         (value-set-p nil))
    ;; by default cursor is finished
    (setf (cursor-state cursor) :finished)
    (cond ((eq response-type rdp:+response-response-type-success-atom+)
           ;; we have an atom, finish the future with the atom value.
           (setf value (cl-rethinkdb-reql::datum-to-lisp (aref (rdp:response response) 0)
                                                         :array-type *sequence-type*
                                                         :object-type *object-type*)
                 value-set-p t))
          ((eq response-type rdp:+response-response-type-success-sequence+)
           ;; we have a sequence, so return a cursor. results accessible via (next ...)
           (setf (cursor-results cursor) (rdp:response response)
                 value cursor
                 value-set-p t))
          ((eq response-type rdp:+response-response-type-success-partial+)
           ;; we have a partial sequence, so return a cursor. results accessible via (next ...)
           (setf (cursor-results cursor) (rdp:response response)
                 value cursor
                 value-set-p t
                 (cursor-state cursor) :partial))
          ((or (find response-type (list rdp:+response-response-type-client-error+
                                         rdp:+response-response-type-compile-error+
                                         rdp:+response-response-type-runtime-error+)))
           ;; some kind of error, signal the future...
           (let ((fail-msg (pb:string-value (rdp::r-str (aref (rdp:response response) 0)))))
             (signal-error future
               (make-instance (cond ((eq response-type rdp:+response-response-type-client-error+)
                                     'query-client-error)
                                    ((eq response-type rdp:+response-response-type-compile-error+)
                                     'query-compile-error)
                                    ((eq response-type rdp:+response-response-type-runtime-error+)
                                     'query-runtime-error))
                              :msg fail-msg
                              :token token))
             ;; because i'm paranoid
             (setf value-set-p nil))))
    (when value-set-p
      (finish future value))
    ;; if the query is finished, remove it from state tracking.
    (when (eq (cursor-state cursor) :finished)
      (remove-cursor cursor)))
  response)

(defun connect (host port &key db use-outdated (read-timeout 5))
  "Connect to a RethinkDB database, optionally specifying the database."
  (let ((future (do-connect host port :read-timeout read-timeout)))
    (alet ((sock future))
      ;; write the version 32-bit integer, little-endian
      (let ((bytes (make-array 4 :element-type '(unsigned-byte 8)))
            (ver cl-rethinkdb-proto:+version-dummy-version-v0-1+))
        (dotimes (i 4)
          (setf (aref bytes i) (ldb (byte 8 (* i 8)) ver)))
        (as:write-socket-data sock bytes))
      ;; setup the socket's options
      (let* ((kv nil)
             (options (make-instance 'connection-options)))
        (when db
          (push `("db" . ,(cl-rethinkdb-reql::db db)) kv))
        (when use-outdated
          (push `("use_outdated" . ,(not (not use-outdated))) kv))
        (setf (conn-kv options) kv
              (socket-data sock) options)))
    future))

(defun disconnect (sock)
  "Disconnect a RethinkDB connection."
  (do-close sock))

(defun serialize-protobuf (protobuf-query)
  "Serialize a RethinkDB protocol buffer, as well as add in the version/size
   bytes to the beginning of the array."
  (let* ((size (pb:octet-size protobuf-query))
         (num-extra-bytes 4)
         (extra-bytes (make-array num-extra-bytes :element-type '(unsigned-byte 8)))
         (buf (make-array size :element-type '(unsigned-byte 8))))
    (pb:serialize protobuf-query buf 0 size)
    ;; write the 32-bit query size
    (dotimes (i 4)
      (setf (aref extra-bytes i) (ldb (byte 8 (* i 8)) size)))
    (cl-async-util:append-array extra-bytes buf)))

(defun run (sock query-form)
  "This function runs the given query, and returns a future that's finished when
   the query response comes in."
  (let* ((future (make-future))
         (token (generate-token))
         (query (make-instance 'rdp:query))
         (cursor (make-instance 'cursor
                                :token token
                                :future future)))
    (setf (rdp:type query) rdp:+query-query-type-start+
          (rdp:query query) query-form)
    ;; setup the global options
    (let* ((options (socket-data sock))
           (kv (conn-kv options)))
      (dolist (opt kv)
        (let ((assoc (make-instance 'rdp:query-assoc-pair)))
          (setf (rdp:key assoc) (pb:string-field (car opt))
                (rdp:val assoc) (cl-rethinkdb-reql::expr (cdr opt)))
          (vector-push-extend assoc (rdp:global-optargs query)))))
    ;; set the token into the query
    (setf (rdp:token query) (the fixnum token))
    ;; save the query with the token so it can be looked up later
    (save-cursor token cursor)
    (forward-errors (future)
      ;; serialize/send/parse the query
      (alet* ((response-bytes (do-send sock (serialize-protobuf query)))
              (response (make-instance 'rdp:response))
              (size (length response-bytes)))
        (pb:merge-from-array response response-bytes 0 size)
        (handler-case
          (parse-response response)
          (error (e)
            (signal-error future e)))))
    (setf (cursor-state cursor) :sent)
    future))

(defun more (sock token)
  "Continue a query."
  (let ((future (make-future))
        (query (make-instance 'rdp:query))
        (cursor (get-cursor token)))
    (setf (rdp:token query) (the fixnum token)
          (rdp:type query) +query-query-type-continue+
          (cursor-future cursor) future)
    (forward-errors (future)
      (alet* ((response-bytes (do-send sock (serialize-protobuf query)))
              (response (make-instance 'rdp:response))
              (size (length response-bytes)))
        (pb:merge-from-array response response-bytes 0 size)
        (handler-case
          (parse-response response)
          (error (e)
            (signal-error future e)))))
    future))

(defun stop (sock cursor)
  "Cleanup a cursor both locally and in the database. Returns a future that is
   finished with *no values* once the stop operation has completed."
  (let ((future (make-future))
        (token (cursor-token cursor)))
    (if (eq (cursor-state cursor) :partial)
        (let ((query (make-instance 'rdp:query)))
          (setf (rdp:token query) (the fixnum token)
                (rdp:type query) +query-query-type-stop+)
          (forward-errors (future)
            (wait-for (do-send sock (serialize-protobuf query))
              (remove-cursor cursor)
              (finish future))))
        (progn
          (remove-cursor cursor)
          (finish future)))
    future))

(defun next (sock cursor)
  "Grab the next result from a cursor. Always returns a future since it may have
   to get more results from the server."
  (let ((future (make-future))
        (num-results (length (cursor-results cursor)))
        (cur-result (cursor-current-result cursor))
        (token (cursor-token cursor)))
    (cond ((< num-results cur-result)
           ;; shouldn't be here, quit calling next!
           (signal-error future (make-instance 'cursor-overshot
                                               :token token
                                               :cursor cursor)))
          ((= cur-result num-results)
           ;; we're out of results. if this was a partial, get more results,
           ;; if not signal a "no more results" error
           (if (eq (cursor-state cursor) :partial)
               ;; moar plz
               (forward-errors (future)
                 (alet ((new-cursor (more sock token)))
                   (finish future (next sock new-cursor))))
               ;; lol none left!!!!
               (signal-error future (make-instance 'cursor-no-more-results
                                                   :token token
                                                   :cursor cursor))))
          (t
           ;; have a local result, send it directly into the future
           (finish future (cl-rethinkdb-reql::datum-to-lisp
                            (aref (cursor-results cursor) cur-result)
                            :array-type *sequence-type*
                            :object-type *object-type*))))
    ;; keep the pointer up to date
    (incf (cursor-current-result cursor))
    future))

(defun has-next (cursor)
  "Determine if a cursor has more results."
  (or (< (cursor-current-result cursor) (length (cursor-results cursor)))
      (and (eq (cursor-state cursor) :partial))))

(defun to-array (sock cursor)
  "Grab ALL results from a cursor. Returns a future finished with the final
   array."
  (let ((future (make-future))
        (token (cursor-token cursor)))
    (labels ((append-results (all-results)
               (if (eq (cursor-state cursor) :partial)
                   (wait-for (more sock token)
                     (append-results (cl-async-util:append-array all-results (cursor-results cursor))))
                   (finish future (cl-rethinkdb-reql::datum-to-lisp
                                    all-results
                                    :array-type *sequence-type*
                                    :object-type *object-type*)))))
      (append-results (cursor-results cursor)))
    future))

(defun each (sock cursor function)
  "Call the given function on every result in the given cursor."
  (let ((future (make-future)))
    (labels ((get-next ()
               (if (has-next cursor)
                   (alet ((result (next sock cursor)))
                     (wait-for (funcall function result)
                       (get-next)))
                   (finish future))))
      (get-next))
    future))

(defun test_ (query-form)
  (as:with-event-loop (:catch-app-errors t)
    (future-handler-case
      (alet ((sock (connect "127.0.0.1" 28015 :read-timeout 1)))
        (future-handler-case
          (multiple-future-bind (res)
              (run sock query-form)
            (format t "---response---")
            (if (cursorp res)
                (alet ((res (to-array sock res)))
                  (pprint res)
                  (format t "~%")
                  ;(disconnect sock)
                  )
                (progn
                  (pprint res)
                  (format t "~%")
                  (disconnect sock))))
          (t (e)
            (format t "Query error: ~a~%" e)
            (disconnect sock))))
      (t (e)
        (format t "Error: ~a~%" e)))))

