(in-package :cl-rethinkdb)

(defun alistp (alist)
  "Determine if the given object is an alist."
  (flet ((true-cdr-p (element)
           (and (consp element)
                (cdr element))))
    (and (listp alist)
         (every #'true-cdr-p alist))))

(defmacro do-list/vector ((bind-val list/vector) &body body)
  "Generifies looping over a list OR vector."
  (let ((val (gensym "list/vector")))
  `(let ((,val ,list/vector))
     (if (vectorp ,val)
         (loop for ,bind-val across ,val do
           ,@body)
         (dolist (,bind-val ,val)
           ,@body)))))

