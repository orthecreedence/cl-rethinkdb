(in-package :cl-rethinkdb)

(let ((query (make-instance 'pb:query)))
  (with-slots (type query token)
    (setf type pb:+query-query-type-start+
          token 1
          query nil))
  (let* ((size (pb:octet-size query))
         (buf (make-array size :element-type '(unsigned-byte 8))))
    (pb:serialize query buf 0 size)
    buf))
    

