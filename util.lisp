(defpackage :cl-rethinkdb-util
  (:use :cl)
  (:export #:alistp
           #:objectp
           #:object-collection-p
           #:alist
           #:object
           #:object-collection
           #:do-list/vector
           #:do-hash/alist))
(in-package :cl-rethinkdb-util)

(defun alistp (alist)
  "Determine if the given object is an alist."
  (flet ((true-cdr-p (element)
           (and (consp element)
                (cdr element))))
    (and (listp alist)
         (every #'true-cdr-p alist))))

(defun objectp (object)
  "Determine if an object is a hash table or alist."
  (subtypep (type-of object) 'object))

(defun object-collection-p (collection)
  "Test if an object is a collection (list or vector) or alists/hashes."
  (and (subtypep (type-of collection) '(or list vector))
       (every #'objectp collection)))

(deftype alist () '(satisfies alistp))
(deftype object () '(or hash-table alist))
(deftype object-collection () '(satisfies object-collection-p))

(defmacro do-list/vector ((bind-val list/vector) &body body)
  "Generifies looping over a list OR vector."
  (let ((val (gensym "list/vector")))
    `(let ((,val ,list/vector))
       (if (vectorp ,val)
           (loop for ,bind-val across ,val do
             ,@body)
           (dolist (,bind-val ,val)
             ,@body)))))

(defmacro do-hash/alist (((bind-key bind-val) hash/alist) &body body)
  "Generifies looping over a hash table or alist."
  (let ((val (gensym "hash/alist"))
        (entry (gensym "alist-entry")))
    `(let ((,val ,hash/alist))
       (if (hash-table-p ,val)
           (loop for ,bind-key being the hash-keys of ,val
                 for ,bind-val being the hash-values of ,val do
             ,@body)
           (dolist (,entry ,val)
             (let ((,bind-key (car ,entry))
                   (,bind-val (cdr ,entry)))
               ,@body))))))

