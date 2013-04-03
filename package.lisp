(defpackage :cl-rethinkdb-proto
  (:use :cl :protocol-buffer)
  (:nicknames :rdp))

(defpackage :cl-rethinkdb
  (:use :cl :cl-rethinkdb-util :cl-rethinkdb-proto :cl-async-future :cl-async)
  (:export ))

