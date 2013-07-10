(in-package :cl-rethinkdb-reql)

(defun term-from-datum (datum &optional options)
  "Wraps a term around a datum."
  (let ((term (make-instance 'term)))
    (setf (type term) +term-term-type-datum+
          (datum term) datum)
    (when options
      (dolist (option options)
        (vector-push-extend (term-assoc (car option) (cdr option)) (optargs term))))
    term))

(defun term-assoc (key value)
  "Creates and returns a term assoc pair, used for optional args."
  (let ((term-assoc (make-instance 'term-assoc-pair)))
    (setf (key term-assoc) (pb:string-field key)
          (val term-assoc) (if (typep value 'term)
                               value
                               (term-from-datum (create-datum value))))
    term-assoc))

(defun create-term (type &optional args options)
  "Create a term object. `type` is the raw protobuf type (ie
   +term-term-type-table+, +term-term-type-insert+, etc). `args` is a list of
   term arguments. They will be wrapped in a datum term if they are not term
   objects. `options` is an alist of kv pairs to set as options in the term."
  (let ((term (make-instance 'term)))
    (setf (type term) type)
    (dolist (arg args)
      (let ((value (if (typep arg 'term)
                       arg
                       (term-from-datum arg))))
        (vector-push-extend value (args term))))
    (when options
      (dolist (option options)
        (vector-push-extend (term-assoc (car option) (cdr option)) (optargs term))))
    term))

(defun is-term (term-type term-obj)
  "Simple term type checker."
  (and (typep term-obj 'term)
       (if (listp term-type)
           (find (type term-obj) term-type)
           (eq (type term-obj) term-type))))

(defun term-array (list/vector)
  "Make a MAKE_ARRAY term from a list/vector."
  (assert (typep list/vector '(or list vector)))
  (create-term +term-term-type-make-array+
               (let ((final nil))
                 (do-list/vector (o list/vector)
                   (push (term-from-datum (create-datum o)) final))
                 (nreverse final))))

(defun term-object (alist)
  "Make a MAKE_OBJ term from an alist."
  (assert (alistp alist))
  (create-term +term-term-type-make-obj+
               '()
               alist))

