(in-package :cl-rethinkdb-reql)

(defmacro r (&body query-form)
  "Wraps query generation in a macro that takes care of pesky function naming
   issues. For instance, the function `count` is reserved in CL, so importing
   cl-rethinkdb-reql:count into your app might mess a lot of things up.
   Instead, you can wrap your queries in (r ...) and use *keywords* for function
   names, and all will be well. For instance:
   
     (r::insert (r::table \"users\") '((\"name\" . \"larry\")))
   
   becomes:
   
     (r (:insert (:table \"users\") '((\"name\" . \"larry\"))))
   
   This allows you to separate CL functions from query functions both logically
   and visually."
  ;; collect all our commands (from defcommand) into a big ol' macrolet form
  ;; that converts keywords into the function equivalents
  (let ((macrolet-forms
          (loop for c being the hash-keys of *commands*
                for k = (intern (cl-ppcre:regex-replace "-[0-9]$" (symbol-name c) "")
                                :keyword)
                collect `(,k (&rest args)
                           `(call ,',k ,@args)))))
                
    `(progn
       (macrolet ((:expr (lisp-obj)
                    `(expr ,lisp-obj))
                  (:binary (base64-string)
                    `(binary ,base64-string))
                  ,@(remove-duplicates macrolet-forms :test (lambda (x y)
                                                              (eq (car x) (car y)))))
         ,@query-form))))


#|
(defun query-builder (form)
  "Takes a query form and turns it into a call tree that builds a query when
   evaled."
  (cond ((and (listp form)
              (keywordp (car form)))
         (cl:append (list 'call
                          (car form))
                    (mapcar 'query-builder (cdr form))))
        ((listp form)
         (mapcar 'query-builder form))
        (t form)))

(defmacro r (query-form)
  "Wraps query generation in a macro that takes care of pesky function naming
   issues. For instance, the function `count` is reserved in CL, so importing
   cl-rethinkdb-reql:count into your app might mess a lot of things up.
   Instead, you can wrap your queries in (r ...) and use *keywords* for function
   names, and all will be well. For instance:
   
     (r::insert (r::table \"users\") '((\"name\" . \"larry\")))
   
   becomes:
   
     (r (:insert (:table \"users\") '((\"name\" . \"larry\"))))
   
   This allows you to separate CL functions from query functions both logically
   and visually."
  (query-builder query-form))

|#

