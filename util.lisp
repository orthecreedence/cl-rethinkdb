(defpackage :cl-rethinkdb-util
  (:use :cl :blackbird)
  (:export #:endian
           #:unendian
           #:forward-errors
           #:do-list/vector
           #:do-hash/alist
           #:jprint))
(in-package :cl-rethinkdb-util)

(defun endian (number num-bytes)
  "Convert a number into N bytes little endian."
  (let ((vec (make-array num-bytes :element-type '(unsigned-byte 8))))
    (dotimes (i num-bytes)
      (let ((val (logand number #xff)))
        (setf (aref vec i) val)
        (setf number (ash number -8))))
    vec))

(defun unendian (bytes num-bytes &key (offset 0))
  "Turns N number of bytes at offset in bytes into an integer."
  (let ((num 0))
    (dotimes (i num-bytes)
      (setf (ldb (byte 8 (* i 8)) num) (aref bytes (+ i offset))))
    num))

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

(defun jprint (obj &optional (stream t))
  (yason:encode obj stream)
  (format stream "~%")
  nil)

