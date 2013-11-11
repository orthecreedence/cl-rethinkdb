(defpackage :cl-rethinkdb-proto
  (:use :protocol-buffer)
  (:nicknames :rdp))

(defpackage :cl-rethinkdb-reql
  (:use :cl :cl-rethinkdb-util :cl-rethinkdb-proto)
  ;; steal some things from CL
  (:shadow #:type

           #:replace
           #:delete
           #:get
           #:map
           #:nth
           #:union
           #:reduce
           #:count
           #:merge
           #:append
           #:set-difference
           #:+
           #:-
           #:*
           #:/
           #:>
           #:>=
           #:<
           #:<=
           #:do
           #:error)
  ;; only export our DSL functions
  (:export #:r
           #:fn)
  (:nicknames :reql))

(defpackage :cl-rethinkdb
  (:use :cl :cl-rethinkdb-util :cl-async-future :cl-rethinkdb-reql)
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

