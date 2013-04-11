(in-package :cl-rethinkdb)

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
     :documentation "Tracks the last time the query changed state."))
  (:documentation
    "The query class holds the state of a query, as well as the future that will
     be finished when the query returns results."))

(defmethod (setf query-state) :after (x (query cursor))
  "Track whenever the state changes in a query."
  (declare (ignore x))
  (setf (cursor-last-change query) (get-internal-real-time)))

(define-condition query-failed (simple-error)
  ((token :reader query-failed-token :initarg :token :initform nil)
   (msg :reader query-failed-msg :initarg :msg :initform ""))
  (:report (lambda (c s) (format s "Query failed (~a): ~a" (query-failed-token c) (query-failed-msg c))))
  (:documentation "A general query failure condition."))

(define-condition query-client-error (query-failed) ()
  (:report (lambda (c s) (format s "Client error: Query (~a): ~a" (query-failed-token c) (query-failed-msg c))))
  (:documentation "A client error condition."))

(define-condition query-compile-error (query-failed) ()
  (:report (lambda (c s) (format s "Query failed to compile (~a): ~a" (query-failed-token c) (query-failed-msg c))))
  (:documentation "A query compile error condition."))
  
(define-condition query-runtime-error (query-failed) ()
  (:report (lambda (c s) (format s "Query runtime error (~a): ~a" (query-failed-token c) (query-failed-msg c))))
  (:documentation "A query runtime error condition."))

(defvar *state* (make-instance 'state)
  "Holds all tracking state for the RethinkDB driver.")

(defun generate-token (&key (state *state*))
  "Generates a new token value for a query."
  (prog1 (state-token state) (incf (state-token state))))

(defun save-query (token query &key (state *state*))
  "Associate a query with a token. Retrievable through get-query."
  (setf (gethash token (active-queries state)) query))

(defun get-query (token &key (state *state*))
  "Grab a query associated with a token. Returns nil if no query exists for that
   token"
  (gethash token (active-queries state)))

(defun remove-query (token &key (state *state*))
  "Remove a query/token from state tracking."
  (remhash token (active-queries state)))

(defun query-gc (token &key (state *state*))
  "Garbage collect a partial query after *query-gc-threshold* number of seconds."
  (when *query-gc-threshold*
    (as:delay
      (lambda () (remove-query token :state state))
      :time *query-gc-threshold*)))

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

(defun parse-response (response &key (array-type :array) (object-type :hash))
  "Parses a RethinkDB response (deserialized into protobuf classes) into a
   lispy/readable format.
   
   Also throws any errors encountered in the response (client error, compile
   error, runtime error)."
  (let* ((response-type (type response))
         (token (token response))
         (query (get-query token))
         (future (cursor-future query))
         (value nil)
         (value-set-p nil))
    ;; by default query is finished
    (setf (cursor-state query) :finished)
    (cond ((eq response-type +response-response-type-success-atom+)
           (setf value (cl-rethinkdb-reql::datum-to-lisp (aref (response response) 0) :array-type array-type :object-type object-type)
                 value-set-p t))
          ((eq response-type +response-response-type-success-sequence+)
           (setf value (cl-rethinkdb-reql::datum-to-lisp (response response) :array-type array-type :object-type object-type)
                 value-set-p t))
          ((eq response-type +response-response-type-success-partial+)
           (setf value (cl-rethinkdb-reql::datum-to-lisp (response response) :array-type array-type :object-type object-type)
                 value-set-p t)
           ;; mark query as a partiel
           (setf (cursor-state query) :partial))
          ((or (find response-type (list +response-response-type-client-error+
                                         +response-response-type-compile-error+
                                         +response-response-type-runtime-error+)))
           (let ((fail-msg (pb:string-value (r-str (aref (response response) 0)))))
             (signal-error future
               (make-instance (cond ((eq response-type +response-response-type-client-error+)
                                     'query-client-error)
                                    ((eq response-type +response-response-type-compile-error+)
                                     'query-compile-error)
                                    ((eq response-type +response-response-type-runtime-error+)
                                     'query-runtime-error))
                              :msg fail-msg
                              :token token)))))
    (when value-set-p
      (finish future value token (lambda () (remove-query token))))
    (if (eq (cursor-state query) :finished)
        ;; if the query is finished, remove it from state tracking.
        (remove-query token)
        ;; if the query is a partial and has more results, let it stick around
        ;; until the query GC threshold (*query-gc-threshold*) passes (seconds)
        (query-gc token)))
  response)

(defun connect (host port &key db (read-timeout 5))
  "Connect to a RethinkDB database, optionally specifying the database."
  ;; TODO: figure out a way to tie a connection to a database (or maybe just
  ;; force people to use (db "mydb") in REQL)
  (declare (ignore db))
  (do-connect host port :read-timeout read-timeout))

(defun disconnect (sock)
  "Disconnect a RethinkDB connection."
  (do-close sock))

(defun next (cursor)
  "Grab the next result from a cursor."
  )

(defun serialize-protobuf (protobuf-query)
  "Serialize a RethinkDB protocol buffer, as well as add in the version/size
   bytes to the beginning of the array."
  (let* ((size (pb:octet-size protobuf-query))
         (num-extra-bytes 8)
         (extra-bytes (make-array num-extra-bytes :element-type '(unsigned-byte 8)))
         (buf (make-array size :element-type '(unsigned-byte 8))))
    (pb:serialize protobuf-query buf 0 size)
    ;; write the version 32-bit integer, little-endian
    (let ((ver cl-rethinkdb-proto:+version-dummy-version-v0-1+))
      (dotimes (i 4)
        (setf (aref extra-bytes i) (ldb (byte 8 (* i 8)) ver))))
    ;; write the 32-bit query size
    (dotimes (i 4)
      (setf (aref extra-bytes (+ i 4)) (ldb (byte 8 (* i 8)) size)))
    (cl-async-util:append-array extra-bytes buf)))

(defun run (sock query-form &key (array-type :array) (object-type :hash))
  "This function runs the given query, and returns a future that's finished when
   the query response comes in."
  (let* ((future (make-future))
         (token (generate-token))
         (query (make-instance 'rdp:query))
         (cursor (make-instance 'cursor
                                :token token
                                :future future)))
    (setf (type query) rdp:+query-query-type-start+
          (query query) query-form)
    ;; set the token into the query
    (setf (token query) (the fixnum token))
    ;; save the query with the token so it can be looked up later
    (save-query token cursor)
    ;; serialize/send the query
    (future-handler-case
      (alet* ((response-bytes (do-send sock (serialize-protobuf query)))
              (response (make-instance 'response))
              (size (length response-bytes)))
        (pb:merge-from-array response response-bytes 0 size)
        (handler-case
          (parse-response response :array-type array-type :object-type object-type)
          (error (e)
            (format t "Unhandled response parsing error: ~a~%" e))))
      ;; forward all errors while sending yo our run future
      (error (e) (signal-error future e)))
    (setf (cursor-state cursor) :sent)
    (values future token)))

(defun stop (conn token)
  "Stop a query."
  (let ((future (make-future))
        (query (make-instance 'rdp:query)))
    (setf (token query) (the fixnum token)
          (type query) +query-query-type-stop+)
    (wait-for (do-send conn (serialize-protobuf query))
      (finish future))
    future))

(defun test (query-form)
  (as:start-event-loop
    (lambda ()
      (future-handler-case
        (alet ((sock (connect "127.0.0.1" 28015)))
          (future-handler-case
            (multiple-future-bind (response token)
                (run sock query-form :array-type :array :object-type :alist)
              (format t "---response(~a)---" token)
              (pprint response)
              (as:close-socket sock))
            (error (e)
              (format t "Query error: ~a~%" e)
              (as:close-socket sock))))
        (error (e)
          (format t "Error: ~a~%" e))))))

(defun names ()
  (let* ((names '("andrew" "bernard" "lola" "mary" "hepatitis"
                  "crendalin burgerhouser" "vinnie" "ralph" "john"
                  "connie" "vlad" "attila" "candie" "mandy" "sandy"
                  "philip" "renny" "stimpy" "moe" "larry" "curly"))
         (num-names (length names)))
    (as:start-event-loop
      (lambda ()
        (future-handler-case
          (alet* ((conn (connect "127.0.0.1" 28015)))
            (labels ((make-user (&optional (i 0))
                       (let ((name (nth (random num-names) names))
                             (age (1+ (random 100))))
                         (format t "Adding user: ~s~%" (list name age))
                         (wait-for
                           (run conn (r (:insert (:table "users") `(("name" . ,name) ("age" . ,age)))))
                           (when (< i 100)
                             (make-user (+ i 1)))))))
              (make-user)))
          (error (e) (format t "Err: ~a~%" e))))
      :catch-app-errors t)))

(defun multi ()
  (as:start-event-loop
    (lambda ()
      (future-handler-case
        (alet ((conn (connect "127.0.0.1" 28015 :read-timeout 1)))
          (multiple-future-bind (res token)
              (run conn (r (:table "omg")) :array-type :list :object-type :alist)
            (format t "res: ~s~%" res)
            (alet ((res (run conn (r (:table "omg")) :array-type :list :object-type :alist)))
              (format t "res: ~s~%" res)
              (disconnect conn))))
        (error (e) (format t "ERR: ~a~%" e))))))

