(in-package :cl-rethinkdb-reql)

(define-condition reql-error (simple-error)
  ((msg :reader msg :initarg :msg :initform ""))
  (:report (lambda (c s) (format s "REQL error: ~a" (msg c))))
  (:documentation "Describes a basic error while building a REQL query."))
  
(define-condition reql-bad-value (reql-error) ()
  (:documentation "Describes an error while setting a value into a query."))

(defun table (table-name &key use-outdated)
  "Create a table object with any given options set."
  (let ((options nil))
    (when use-outdated
      (push '("use_outdated" . t) options))
    (create-term +term-term-type-table+
                 (list (create-datum table-name))
                 options)))

(defun insert (table value)
  "Create an insert query, given a table object and a set of values. The table
   object must be one returned by the table function, but the values must not
   be query objects (ie datum/term objects) because those will be created
   automatically by insert.
   
   The value can be a hash table (or alist), or an array of hashes/alists (in
   the case of a multi-insert)."
  (let ((insert-term (make-instance 'term)))
    (setf (type insert-term) +term-term-type-insert+)
    (vector-push-extend table (args insert-term))
    (let ((term (cond ((or (hash-table-p value)
                           (alistp value))
                       (datum-term (create-datum value)))
                      ((subtypep (type-of value) '(or list vector))
                       (let ((terms nil))
                         (do-list/vector (obj value)
                           (format t "obj: ~a~%" obj)
                           (push (datum-term (create-datum obj)) terms))
                         (create-term +term-term-type-make-array+
                                      (reverse terms))))
                      (t
                       (error 'reql-bad-value
                              :msg "Value must be a hashtable, alist, or list/vector of hashtables or alists.")))))
      (format t "~a~%" term)
      (vector-push-extend term (args insert-term)))
    insert-term))

(progn
(insert (table "omg")
        '((("name" . "leonard"))))
(insert (table "omg")
        '(("name" . "leonard")))
nil)

(create-datum '(("name" . "leonard")))
