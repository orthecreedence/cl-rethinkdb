(in-package :cl-rethinkdb-reql)

(defun alistp (alist)
  "Determine if the given object is an alist."
  (flet ((true-cdr-p (element)
           (and (consp element)
                (cdr element))))
    (and (not (null alist))
         (listp alist)
         (every #'true-cdr-p alist))))

(deftype alist () '(satisfies alistp))

