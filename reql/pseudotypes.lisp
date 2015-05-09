(in-package :cl-rethinkdb-reql)

(define-condition reql-error (simple-error)
  ((msg :reader reql-error-msg :initarg :msg :initform ""))
  (:report (lambda (c s) (format s "REQL error: ~a" (reql-error-msg c))))
  (:documentation "A general reql error"))

(defun convert-pseudotypes (obj)
  "Handle pseudotypes."
  (let ((type (and (hash-table-p obj)
                   (gethash "$reql_type$" obj)) ))
    (format t "type: ~a~%" type)
    (unless type
      (return-from convert-pseudotypes obj))
    (case (intern (string-upcase type) :keyword)
      (:time
        (let ((epoch (gethash "epoch_time" obj)))
          (unless epoch
            (cl:error (make-instance 'reql-error :msg "pseudotype TIME missing `epoch_time` field")))
          epoch))
      (:grouped_data
        (mapcar (lambda (x) (hu:hash ("group" (elt x 0)) ("reduction" (elt x 1))))
                (gethash "data" obj)))
      (:binary
        (let ((data (gethash "data" obj)))
          (unless data
            (cl:error (make-instance 'reql-error :msg "pseudotype BINARY missing `data` field")))
          (cl-base64:base64-string-to-usb8-array data)))
      (t obj))))

(defun convert-pseudotypes-recursive (obj)
  "Recursively handle RethinkDB's pseudotypes in returned objects."
  (cond ((and (vectorp obj)
              (not (stringp obj)))
         (loop for x across obj
               for i from 0 do
           (setf (aref obj x) (convert-pseudotypes-recursive x))))
        ((listp obj)
         (loop for x in obj
               for i from 0 collect (convert-pseudotypes-recursive x)))
        ((hash-table-p obj)
         (loop for k being the hash-keys of obj
               for v being the hash-values of obj do
           (setf (gethash k obj) (convert-pseudotypes-recursive v)))
         (setf obj (convert-pseudotypes obj))))
  obj)

