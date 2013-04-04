;;; NOTE: this file contains the base minimum number of functions that it can to
;;; support connecting/sending/receiving. The idea is to eventually allow socket
;;; types other than cl-async (usocket, iolib) if desired.
;;;
;;; The contained functions may be trimmed down even more once other connection
;;; types are added to reduce duplication.

(in-package :cl-rethinkdb)

(defun do-connect (host port &key (read-timeout 5))
  "Create a connection to the given host/port, and optionally db."
  (let ((future (make-future))
        (sock nil))
    ;; make sure sock is async so people aren't tempted to depend on a sync op
    (as:delay (lambda () (finish future sock)))
    (setf sock (as:tcp-connect host port 
                 nil
                 (lambda (ev) (signal-error future ev))
                 :read-timeout read-timeout))))

(defun send-query (sock query)
  "Send a query to the server over the given socket using the given token.
   Returns a future that is finished with the full byte-array of the response
   once the full response returns."
  (let* ((future (make-future))
         (size (pb:octet-size query))
         (num-extra-bytes 8)
         (extra-bytes (make-array num-extra-bytes :element-type '(unsigned-byte 8)))
         (buf (make-array size :element-type '(unsigned-byte 8)))
         ;; create a lambda that can be called multiple times with chunks of
         ;; response data, returning the full data (byte array) only when all
         ;; of the response has been received.
         (response-handler (make-response-handler)))
    (pb:serialize query buf 0 size)
    ;; write the version 32-bit integer, little-endian
    (let ((ver cl-rethinkdb-proto:+version-dummy-version-v0-1+))
      (dotimes (i 4)
        (setf (aref extra-bytes i) (ldb (byte 8 (* i 8)) ver))))
    ;; write the 32-bit query size
    (dotimes (i 4)
      (setf (aref extra-bytes (+ i 4)) (ldb (byte 8 (* i 8)) size)))
    (let ((final (cl-async-util:append-array extra-bytes buf)))
      (as:write-socket-data sock final
        :read-cb (lambda (sock data)
                   (declare (ignore sock))
                   (let ((full-response-bytes (funcall response-handler data)))
                     (when full-response-bytes
                       (finish future full-response-bytes))))
        :event-cb (lambda (ev)
                    (let ((ev-type (type-of ev)))
                      (when (or (not (subtypep ev-type 'as:event-info))
                                (subtypep ev-type 'as:event-error))
                        (signal-error future ev))))))
    future))

(defun continue-query (sock query)
  "Request to continue a query given a socket and the token of an existing
   query."
  )

(defun stop-query (sock query)
  "Request to stop a query given a socket and the token of an existing query."
  )
