(defpackage :cl-rethinkdb-reql
  (:use :cl :cl-rethinkdb-util)
  ;; steal some things from CL
  (:shadow #:make-array
           #:replace
           #:delete
           #:get
           #:map
           #:nth
           #:union
           #:reduce
           #:count
           #:min
           #:max
           #:merge
           #:append
           #:type-of
           #:set-difference
           #:+
           #:-
           #:*
           #:/
           #:>
           #:>=
           #:<
           #:<=
           #:time
           #:do
           #:error
           #:random
           #:fill)
  ;; only export our DSL functions
  (:export #:r
           #:fn
           #:convert-pseudotypes-recursive)
  (:nicknames :reql))

(defpackage :cl-rethinkdb
  (:use :cl :cl-rethinkdb-util :blackbird :cl-rethinkdb-reql)
  (:export :*state*
           :state
           
           :*sequence-type*
           :*object-type*
           
           :query-error
           :query-error-msg
           :query-client-error
           :query-compile-error
           :query-runtime-error

           :cursor-error
           :cursor-no-more-results
           :cursor-overshot

           :cursor
           :cursorp

           :connect
           :disconnect

           :run
           :wait-complete
           :next
           :has-next
           :to-array
           :each
           :stop
           :stop/disconnect

           ;; export ReQL DSL functions
           :r
           :fn)
  (:nicknames :r))

