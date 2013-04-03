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

(defmacro defterm (name type (&rest args) (&rest options) &optional docstring)
  (let ((option (gensym "option")))
    `(defun ,name (,@(loop for a in args collect (car a))
                    &key ,@(loop for k in options collect (append (list (caar k)) (cdr k))))
       ,docstring
       `(let ((term (make-instance 'term))
              (options nil))
          (setf (type term) ,type)
          ;; type check
          ,@(loop for a across args
                  collect `(assert (typep ,(car a) ,(cadr a))))
          ,@(loop for k across options
                  collect `(assert (typep ,(caar k) ,(cadar k))))
          (dolist (,option ,options)
            (when ,(caddr option)
              (push (cons ,(caddar option) ,(caar option)) options)))
          ))))

(defun insert (table value &key upsert)
  "Create an insert query, given a table object and a set of values. The table
   object must be one returned by the table function, but the values must not
   be query objects (ie datum/term objects) because those will be created
   automatically by insert.
   
   The value can be a hash table (or alist), or an array of hashes/alists (in
   the case of a multi-insert)."
  (assert (and (typep table 'term)
               (eq (type table) +term-term-type-table+)))
  (assert (typep value '(or object-collection object)))
  (assert (typep upsert 'boolean))
  (let ((value (cond ((object-collection-p value)
                      (let ((terms nil))
                        (do-list/vector (obj value)
                          (push (datum-term (create-datum obj)) terms))
                        (create-term +term-term-type-make-array+
                                     (reverse terms))))
                     ((objectp value)
                      (datum-term (create-datum value)))
                     (t
                      (error 'reql-bad-value
                             :msg "Value must be a hashtable, alist, or list/vector of hashtables or alists.")))))
    (create-term +term-term-type-insert+
                 (list table value)
                 (when upsert '(("upsert" . t))))))

