(in-package :cl-rethinkdb-reql)

(defvar *varnum* 0
  "Used to track lambda variables in functions.")

(defun generate-fn-var ()
  "Returns unique variable 'names' for anonymous functions."
  (incf *varnum*))

(defun make-function (var-numbers body-term)
  "Create a RQL function out of a list of variable numbers and a body term."
  (create-term
    +term-term-type-func+
    (list (create-term
            +term-term-type-make-array+
            (loop for x in var-numbers collect (wrap-in-term x)))
          body-term)))

(defun var (number)
  "Given a variable number, return the term referencing that variable."
  (assert (realp number))
  (create-term +term-term-type-var+ (list (wrap-in-term number))))

(defun num-args (reql-function)
  "Determine the number of arguments in a REQL function."
  (when (is-term +term-term-type-func+ reql-function)
    (length (args (aref (args reql-function) 0)))))

(defmacro fn (args &body body)
  (let ((arg-nums (loop for nil in args collect (generate-fn-var))))
    `(symbol-macrolet (,@(loop for a in args
                               for n in arg-nums
                               collect (list a `(var ,n))))
       (make-function ',arg-nums (wrap-in-term (r ,@body))))))

