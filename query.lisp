(in-package :cl-rethinkdb)

(define-condition query-error (simple-error)
  ((token :reader query-error-token :initarg :token :initform nil)
   (query :reader query-error-query :initarg :query :initform nil)
   (backtrace :reader query-error-backtrace :initarg :backtrace :initform nil)
   (msg :reader query-error-msg :initarg :msg :initform ""))
  (:report (lambda (c s) (format s "Query failed (~a): ~a~%---~%~a" (query-error-token c) (query-error-msg c) (query-error-query c))))
  (:documentation "A general query failure condition."))

(define-condition query-client-error (query-error) ()
  (:report (lambda (c s) (format s "Client error: Query (~a): ~a~%---~%~a" (query-error-token c) (query-error-msg c) (query-error-query c))))
  (:documentation "A client error condition."))

(define-condition query-compile-error (query-error) ()
  (:report (lambda (c s) (format s "Query failed to compile (~a): ~a~%---~%~a" (query-error-token c) (query-error-msg c) (query-error-query c))))
  (:documentation "A query compile error condition."))
  
(define-condition query-runtime-error (query-error) ()
  (:report (lambda (c s) (format s "Query runtime error (~a): ~a~%---~%~a" (query-error-token c) (query-error-msg c) (query-error-query c))))
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
     :documentation "Tracks which record the cursor points to.")
   (debug :accessor cursor-debug :initarg :debug :initform nil
     :documentation "Holds freeform debug info for this cursor."))
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
  (let ((token nil)
        (response-buffer (fast-io:make-output-buffer))
        (response-size nil))
    (lambda (bytes)
      (let* ((bufsize (fast-io:buffer-position response-buffer))
             (end (if response-size
                      (- (+ response-size 4 8)
                         bufsize)
                      (length bytes)))
             (end (min end (length bytes))))
        (fast-io:fast-write-sequence bytes
                                     response-buffer
                                     0
                                     end))
      (when (and (not token)
                 (<= 8 (fast-io:buffer-position response-buffer)))
        (setf token (unendian (fast-io:finish-output-buffer response-buffer) 8)))
      (when (and (not response-size)
                 (<= 12 (fast-io:buffer-position response-buffer)))
        (setf response-size (unendian (fast-io:finish-output-buffer response-buffer) 4 :offset 8)))
      ;; calculate current size minus token/size bytes
      (let ((cursize (- (fast-io:buffer-position response-buffer) (+ 4 8))))
        (when (and response-size
                   (<= response-size cursize))
          (let ((output (fast-io:finish-output-buffer response-buffer)))
            output))))))

(defun parse-response (response-bytes)
  "Given a full response byte array, parse it, find the attached cursor (by
   token), and either resolve/reject the cursor's promise with the return of the
   query."
  (let* ((token (unendian response-bytes 8))
         (response-unparsed (subseq response-bytes (+ 8 4)))
         (response (yason:parse (babel:octets-to-string response-unparsed)
                                :json-arrays-as-vectors t))
         (cursor (get-cursor token))
         (query-form (cursor-debug cursor))
         (promise-data (cursor-future cursor))
         (resolver (car promise-data))
         (rejecter (cdr promise-data))
         (response-type (gethash "t" response))
         (value (gethash "r" response))
         (value-set-p nil)
         (backtrace (gethash "b" response))
         (profile (gethash "p" response)))
    (vom:info "recv: ~a" (babel:octets-to-string response-unparsed))
    (setf (cursor-state cursor) :finished)
    (cond ((eq response-type +rdb-response-atom+)
           (setf value (aref value 0)
                 value-set-p t))
          ((eq response-type +rdb-response-sequence+)
           ;; we have a sequence, so return a cursor. results accessible via (next ...)
           (setf (cursor-results cursor) value
                 value cursor
                 value-set-p t))
          ((eq response-type +rdb-response-partial+)
           ;; we have a partial sequence, so return a cursor. results accessible via (next ...)
           (setf (cursor-results cursor) value
                 value cursor
                 value-set-p t
                 (cursor-state cursor) :partial))
          ((eq response-type +rdb-response-wait-complete+)
           ;; we have a NOREPLY_WAIT response. just finish.
           (setf (cursor-results cursor) value
                 value t
                 value-set-p t))
          ((or (find response-type (list +rdb-response-client-error+
                                         +rdb-response-compile-error+
                                         +rdb-response-runtime-error+)))
           ;; some kind of error, signal the future...
           (let* ((fail-msg (aref value 0))
                  (error-obj (make-instance (cond ((eq response-type +rdb-response-client-error+)
                                                   'query-client-error)
                                                  ((eq response-type +rdb-response-compile-error+)
                                                   'query-compile-error)
                                                  ((eq response-type +rdb-response-runtime-error+)
                                                   'query-runtime-error))
                                            :msg fail-msg
                                            :query query-form
                                            :backtrace backtrace)))
             (catcher
               (error error-obj)
               ((or error simple-error) (e) (funcall rejecter e)))
             ;; because i'm paranoid
             (setf value-set-p nil))))
    (when value-set-p
      (funcall resolver value profile))
    ;; if the query is finished, remove it from state tracking.
    (when (eq (cursor-state cursor) :finished)
      (remove-cursor cursor))))

(defun connect (host port &key db use-outdated noreply profile (read-timeout 5) auth)
  "Connect to a RethinkDB database, optionally specifying the database."
  (alet* ((sock (do-connect host port :read-timeout read-timeout)))
    ;; write the version 32-bit integer, little-endian
    (sock-write sock (endian +proto-version+ 4))
    (if auth
        (let ((auth (if (stringp auth)
                        (babel:string-to-octets auth)
                        auth)))
          (sock-write sock (endian (length auth) 4))
          (sock-write sock auth))
        (sock-write sock (endian 0 4)))
    (sock-write sock (endian +proto-json+ 4))
    ;; setup the socket's options
    (let* ((kv nil)
           (options (make-instance 'connection-options)))
      (when db
        (push `("db" . ,(cl-rethinkdb-reql::db db)) kv))
      (when use-outdated
        (push `("use_outdated" . ,(not (not use-outdated))) kv))
      (when noreply
        (push `("noreply" . ,(not (not noreply))) kv))
      (when profile
        (push '("profile" . t) kv))
      (setf (conn-kv options) kv
            (socket-data sock) options))
    (finalize-connect sock)))

(defun disconnect (sock)
  "Disconnect a RethinkDB connection."
  (do-close sock))

(defun serialize-query (query)
  "Turn a query into a byte array."
  (babel:string-to-octets
    (with-output-to-string (s)
      (yason:encode query s))))

(defun run (sock query-form)
  "This function runs the given query, and returns a future that's finished when
   the query response comes in."
  (with-promise (resolve reject :resolve-fn resolver :reject-fn rejecter)
    (let* ((token (generate-token))
           (query-options (hu:hash))
           (query (list +proto-query-start+ query-form query-options))
           (cursor (make-instance 'cursor
                                  :token token
                                  :future (cons resolver rejecter)
                                  :debug query-form))
           (options (socket-data sock))
           (kv (conn-kv options)))
      (dolist (opt kv)
        (setf (gethash (car opt) query-options) (cdr opt)))
      (save-cursor token cursor)
      (let ((serialized (serialize-query query)))
        (sock-write sock (endian token 8))
        (sock-write sock (endian (length serialized) 4))
        (sock-write sock serialized)
        (finalize-query sock)
        (vom:info "send: run: ~a" (babel:octets-to-string serialized))
        (setf (cursor-state cursor) :sent)))))

(defun wait-complete (sock)
  "Wait for noreply => t queries to come back LOL."
  (with-promise (resolve reject :resolve-fn resolver :reject-fn rejecter)
    (let* ((token (generate-token))
           (query (list +proto-query-wait+))
           (cursor (make-instance 'cursor
                                  :token token
                                  :future (cons resolver rejecter))))
      (save-cursor token cursor)
      (let ((serialized (serialize-query query)))
        (sock-write sock (endian token 8))
        (sock-write sock (endian (length serialized) 4))
        (sock-write sock serialized)
        (vom:info "send: wait: ~a" (babel:octets-to-string serialized))
        (finalize-query sock)
        (setf (cursor-state cursor) :wait)))))

(defun more (sock token)
  "Continue a query."
  (with-promise (resolve reject :resolve-fn resolver :reject-fn rejecter)
    (let* ((query (list +proto-query-continue+))
           (cursor (get-cursor token)))
      ;; replace the cursor's promise resolver
      (setf (cursor-future cursor) (cons resolver rejecter))
      (save-cursor token cursor)
      (let ((serialized (serialize-query query)))
        (sock-write sock (endian token 8))
        (sock-write sock (endian (length serialized) 4))
        (sock-write sock serialized)
        (finalize-query sock)
        (vom:info "send: more: ~a" (babel:octets-to-string serialized))
        (setf (cursor-state cursor) :more)))))

(defun stop (sock cursor)
  "Cleanup a cursor both locally and in the database. Returns a future that is
   finished with *no values* once the stop operation has completed."
  (with-promise (resolve reject :resolve-fn resolver :reject-fn rejecter)
    (let* ((query (list +proto-query-stop+))
           (token (cursor-token cursor)))
      ;; replace the cursor's promise resolver
      (setf (cursor-future cursor) (cons resolver rejecter))
      (if (eq (cursor-state cursor) :partial)
          (let ((serialized (serialize-query query)))
            (sock-write sock (endian token 8))
            (sock-write sock (endian (length serialized) 4))
            (sock-write sock serialized)
            (finalize-query sock)
            (vom:info "send: stop: ~a" (babel:octets-to-string serialized))
            (setf (cursor-state cursor) :stop))
          (progn
            (remove-cursor cursor)
            (resolve))))))

(defun stop/disconnect (sock cursor)
  "Call stop on a cursor and disconnect the passed socket."
  (with-promise (resolve reject)
    (if (cursorp cursor)
        (catcher
          (wait (stop sock cursor)
            (resolve (disconnect sock)))
          (error (e) (reject e)))
        (resolve (disconnect sock)))))

(defun next (sock cursor)
  "Grab the next result from a cursor. Always returns a future since it may have
   to get more results from the server."
  (with-promise (resolve reject)
    (let ((num-results (length (cursor-results cursor)))
          (cur-result (cursor-current-result cursor))
          (token (cursor-token cursor)))
      (cond ((< num-results cur-result)
             ;; shouldn't be here, quit calling next!
             (reject (make-instance 'cursor-overshot
                                    :token token
                                    :cursor cursor)))
            ((= cur-result num-results)
             ;; we're out of results. if this was a partial, get more results,
             ;; if not signal a "no more results" error
             (catcher
               (if (eq (cursor-state cursor) :partial)
                   ;; moar plz
                   (alet* ((new-cursor (more sock token)))
                     (resolve (next sock new-cursor)))
                   ;; lol none left!!!!
                   (reject (make-instance 'cursor-no-more-results
                                          :token token
                                          :cursor cursor)))
               (error (e) (reject e))))
            (t
             ;; have a local result, send it directly into the future
             (resolve (aref (cursor-results cursor) cur-result))))
      ;; keep the pointer up to date
      (incf (cursor-current-result cursor)))))

(defun has-next (cursor)
  "Determine if a cursor has more results."
  (and (cursorp cursor)
       (or (< (cursor-current-result cursor) (length (cursor-results cursor)))
           (eq (cursor-state cursor) :partial))))

(defun to-array (sock cursor)
  "Grab ALL results from a cursor. Returns a future finished with the final
   array."
  (with-promise (resolve reject)
    (cond ((cursorp cursor)
           (let ((token (cursor-token cursor)))
             (labels ((append-results (all-results)
                        (catcher
                          (if (eq (cursor-state cursor) :partial)
                              (wait (more sock token)
                                (append-results (cl-async-util:append-array all-results (cursor-results cursor))))
                              (resolve all-results))
                          (error (e) (reject e)))))
               (append-results (cursor-results cursor)))))
          ((arrayp cursor)
           (resolve cursor))
          (t (error (format nil "to-array: bad cursor given: ~a" cursor))))))

(defun each (sock cursor function)
  "Call the given function on every result in the given cursor."
  (with-promise (resolve reject)
    (labels ((get-next ()
               (catcher
                 (if (has-next cursor)
                     (alet* ((result (next sock cursor)))
                       (wait (funcall function result)
                         (get-next)))
                     (resolve))
                 (error (e) (reject e)))))
      (get-next))))

(defun test_ (query-form)
  (as:with-event-loop (:catch-app-errors nil)
    (let ((main-sock nil))
      (finally
        (catcher
          (alet* ((sock (connect "127.0.0.1" 28015 :db "test" :read-timeout 5)))
            (setf main-sock sock)
            (multiple-promise-bind (res)
                (run sock query-form)
              (format t "---response---~%")
              (if (cursorp res)
                  (alet* ((res (to-array sock res)))
                    (jprint res))
                  (jprint res))
              (format t "~%")))
          (error (e) (format t "(err) ~a~%" e)))
        (disconnect main-sock)))))

