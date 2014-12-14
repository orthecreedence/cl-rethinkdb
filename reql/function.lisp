(in-package :cl-rethinkdb-reql)

(defvar *varnum* 0
  "Used to track lambda variables in functions.")

(defun generate-fn-var ()
  "Returns 'unique' variable 'names' for 'anonymous' 'functions' ;) ;) if you
   know what I mean heh heh ;) ;) ;)."
  (incf *varnum*))

(defmacro fn (args &body body)
  "Makes creating anonymous REQL functions easy. Takes a list of arguments (this
   is not a real lambda list, just a flat list of args) and wraps the body in
   the REQL-generating form 'r'."
  (let ((arg-nums (loop for nil in args collect (generate-fn-var))))
    `(symbol-macrolet (,@(loop for a in args
                               for n in arg-nums
                               collect (list a `(var ,n))))
       (func ',(apply 'make-array arg-nums) (r ,@body)))))

