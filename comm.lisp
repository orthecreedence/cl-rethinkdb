(in-package :cl-rethinkdb)

(defvar *token* 0
  "Holds the next query token value.")

(defun generate-token ()
  "Generates a new token value for a query."
  (prog1 *token* (incf *token*)))

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
  response)

(defun connect (host port &key db (read-timeout 5))
  "Connect to a RethinkDB database, optionally specifying the database."
  ;; TODO: figure out a way to tie a connection to a database (or maybe just
  ;; force people to use (db "mydb") in REQL)
  (declare (ignore db))
  (do-connect host port :read-timeout read-timeout))

(defun run (sock query)
  "This function runs the given query, and returns a future that's finished when
   the query response comes in."
  (let ((future (make-future))
        (token (generate-token)))
    ;; set the token into the query
    (setf (token query) (the fixnum token))
    ;; send the query, 
    (alet ((response-bytes (send-query sock query)))
      (let* ((response (make-instance 'response))
             (size (length response-bytes)))
        (pb:merge-from-array response response-bytes 0 size)
        (handler-case
          (finish future (parse-response response) token)
          (error (e)
            (format t "Unhandled response parsing error: ~a~%" e)))))
    (values future token)))

(defun test ()
  (as:start-event-loop
    (lambda ()
      (alet ((sock (connect "127.0.0.1" 28015)))
        (let ((query (make-instance 'query)))
          (setf (type query) rdp:+query-query-type-start+
                (query query) (r::table-create (r::db "test") "omg"))
          (multiple-future-bind (response token)
              (run sock query)
            (format t "---response(~a)---~%~s~%" token response)
            (as:close-socket sock))
          )))))

