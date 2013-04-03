(in-package :cl-rethinkdb)

(defun test ()
  (let ((query (make-instance 'query)))
    (with-slots (type query token) query
      (setf type rdp:+query-query-type-start+
            token 1
            query nil))
    (let* ((size (pb:octet-size query))
           (buf (make-array size :element-type '(unsigned-byte 8))))
      (format t "size: ~a~%" size)
      (pb:serialize query buf 0 size)
      buf)))

