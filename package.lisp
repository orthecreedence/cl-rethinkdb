(defpackage :cl-rethinkdb-proto
  (:use :cl :protocol-buffer)
  (:nicknames :rdp))

;; this is defined later, but we want it available for the next defpackage
(defpackage :cl-rethinkdb-reql (:use :cl))

(defpackage :cl-rethinkdb
  (:use :cl :cl-rethinkdb-util :cl-rethinkdb-proto :cl-async-future :cl-rethinkdb-reql)
  (:export :query-error
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
           :stop))

