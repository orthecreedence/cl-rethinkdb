(in-package :cl-rethinkdb-reql)

(defun alistp (alist)
  "Determine if the given object is an alist."
  (flet ((true-cdr-p (element)
           (and (consp element)
                (cdr element))))
    (and (not (null alist))
         (listp alist)
         (every #'true-cdr-p alist))))

(defun objectp (object)
  "Determine if an object is a hash table or alist."
  (typep object 'object))

(defun object-collection-p (collection)
  "Test if an object is a collection (list or vector) or alists/hashes."
  (and (subtypep (type-of collection) '(or list vector))
       (every #'objectp collection)))

(deftype alist () '(satisfies alistp))
(deftype object () '(or hash-table alist))
(deftype object-collection () '(satisfies object-collection-p))

