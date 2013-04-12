;;; NOTE: this file contains the base minimum number of functions that it can to
;;; support connecting/sending/receiving. The idea is to eventually allow socket
;;; types other than cl-async (usocket, iolib) if desired.

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

(defun do-send (sock bytes)
  "Send a query to the server over the given socket using the given token.
   Returns a future that is finished with the full byte-array of the response
   once the full response returns."
  (let* ((future (make-future))
         ;; create a lambda that can be called multiple times with chunks of
         ;; response data, returning the full data (byte array) only when all
         ;; of the response has been received.
         (response-handler (make-response-handler)))
    (as:write-socket-data sock bytes
      :read-cb (lambda (sock data)
                 (declare (ignore sock))
                 (let ((full-response-bytes (funcall response-handler data)))
                   (when full-response-bytes
                     (finish future full-response-bytes))))
      :event-cb (lambda (ev)
                  (let ((ev-type (type-of ev)))
                    (when (or (not (subtypep ev-type 'as:event-info))
                              (subtypep ev-type 'as:event-error))
                      (signal-error future ev)))))
    future))

(defun do-close (sock)
  "Close the given socket."
  (as:close-socket sock))

(defmacro socket-data (socket)
  "Allow storing of arbitrary data with a socket."
  `(as:socket-data ,socket))
  
