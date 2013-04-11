(defpackage :cl-rethinkdb-proto
  (:use :cl :protocol-buffer)
  (:nicknames :rdp))

(defpackage :cl-rethinkdb-reql (:use :cl))

(defpackage :cl-rethinkdb
  (:use :cl :cl-rethinkdb-util :cl-rethinkdb-proto :cl-async-future :cl-rethinkdb-reql)
  (:export :connect
           :run
           :disconnect))

