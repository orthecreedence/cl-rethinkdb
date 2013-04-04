(in-package :cl-rethinkdb-reql)

(defclass javascript-fn ()
  ((arguments :reader js-args :initarg :args :initform nil)
   (body :reader js-body :initarg :body :initform nil))
  (:documentation "Describes a javascript function object."))

(defmethod print-object ((fn javascript-fn) s)
  (format s "#<javascript-function (~a args)>"
          (length (js-args fn))))

(defgeneric num-args (javascript-fn)
  (:documentation
    "Returns the number of arguments a javascript function contains."))

(defmethod num-args ((fn javascript-fn))
  (length (js-args fn)))

(defmacro fn (args &body body)
  "Abstracts creation of javascript functions from lisp code. Will eventually be
   translated into a javascript script via parenscript."
  `(make-instance 'javascript-fn
                  :args ',args
                  :body '(,@body)))

(defgeneric fn-to-str (javascript-fn)
  (:documentation
    "Converts a javascrtipt function object into a string (using parenscript)"))

(defmethod fn-to-str ((fn javascript-fn))
  (ps:ps* `(lambda ,(js-args fn)
             ,@(js-body fn))))

