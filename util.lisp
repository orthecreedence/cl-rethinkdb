(defpackage :cl-rethinkdb-util
  (:use :cl :cl-async-future)
  (:export #:forward-errors
           #:do-list/vector
           #:do-hash/alist))
(in-package :cl-rethinkdb-util)

(defmacro forward-errors ((future) body-form)
  "Forward all errors encountered in a form to the given future."
  `(future-handler-case
     ,body-form
     (error (e) (signal-error ,future e))))

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

