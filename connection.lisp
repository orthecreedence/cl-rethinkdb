;;; NOTE: this file contains the base minimum number of functions that it can to
;;; support connecting/sending/receiving. The idea is to eventually allow socket
;;; types other than cl-async (usocket, iolib) if desired.

(in-package :cl-rethinkdb)

(define-condition connect-error (simple-error)
  ((msg :reader connect-error-msg :initarg :msg :initform ""))
  (:report (lambda (c s) (format s "Connection error: ~a" (connect-error-msg c))))
  (:documentation "A general connection error condition."))

(defparameter *empty* (make-array 0 :element-type '(unsigned-byte 8))
  "An empty data set for setting callbacks. Perhaps a cl-async API for this is
   in order??")

(defun do-connect (host port &key (read-timeout 5))
  "Create a connection to the given host/port, and optionally db."
  (with-promise (resolve reject)
    (let ((sock (as:tcp-connect host port 
                  nil
                  :event-cb (lambda (ev) (reject ev))
                  :read-timeout read-timeout)))
      (as:with-delay (0)
        (resolve sock)))))

(defun finalize-connect (sock)
  "Make sure a connection to the DB was successful."
  (with-promise (resolve reject)
    (as:write-socket-data sock *empty*
      :read-cb (lambda (sock data)
                 (let ((msg (babel:octets-to-string data)))
                   (if (string= (subseq msg 0 7) "SUCCESS")
                       (resolve sock)
                       (reject (make-instance 'connect-error
                                              :msg (format nil "bad connect: ~a~%" msg))))))
      :event-cb (lambda (ev)
                  (let ((ev-type (type-of ev)))
                    (when (or (not (subtypep ev-type 'as:event-info))
                              (subtypep ev-type 'as:event-error))
                      (reject ev)))))))

(defun sock-write (sock bytes)
  "Send data on a rethinkdb connection."
  (as:write-socket-data sock bytes))

(defun finalize-query (sock)
  "Make sure a socket that just had query data sent over it is ready to handle
   the response."
  (with-promise (resolve reject)
    (let ((response-handler (make-response-handler)))
      (as:write-socket-data sock *empty*
        :read-cb (lambda (sock data)
                   (declare (ignore sock))
                   (let ((full-response-bytes (funcall response-handler data)))
                     (when full-response-bytes
                       (parse-response full-response-bytes))))
        :event-cb (lambda (ev)
                    (let ((ev-type (type-of ev)))
                      (when (or (not (subtypep ev-type 'as:event-info))
                                (subtypep ev-type 'as:event-error))
                        (reject ev))))))))

(defun do-close (sock)
  "Close the given socket."
  (as:close-socket sock))

(defmacro socket-data (socket)
  "Allow storing of arbitrary data with a socket."
  `(as:socket-data ,socket))
  
