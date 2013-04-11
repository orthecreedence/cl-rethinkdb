(defpackage :cl-rethinkdb-reql
  (:use :cl :cl-rethinkdb-util :cl-rethinkdb-proto :cl-rethinkdb)
  (:shadow #:replace
           #:delete
           #:get
           #:map
           #:nth
           #:union
           #:reduce
           #:count
           #:merge
           #:append
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
  (:export #:r
           #:fn))

