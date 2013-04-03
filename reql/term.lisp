(in-package :cl-rethinkdb-reql)

(defun term-assoc (key value)
  "Creates and returns a term assoc pair, used for optional args."
  (let ((term-assoc (make-instance 'term-assoc-pair)))
    (setf (key term-assoc) (pb:string-field key)
          (val term-assoc) (datum-term (create-datum value)))
    term-assoc))

(defun create-term (type args &optional options)
  "Create a term object. `type` is the raw protobuf type (ie
   +term-term-type-table+, +term-term-type-insert+, etc). `args` is a list of
   term arguments. They will be wrapped in a datum term if they are not term
   objects. `options` is an alist of kv pairs to set as options in the term."
  (let ((term (make-instance 'term)))
    (setf (type term) type)
    (dolist (arg args)
      (let ((value (if (subtypep (type-of arg) 'term)
                       arg
                       (datum-term arg))))
        (vector-push-extend value (args term))))
    (when options
      (dolist (option options)
        (vector-push-extend (term-assoc (car option) (cdr option)) (optargs term))))
    term))

