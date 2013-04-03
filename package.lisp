(defpackage :cl-rethinkdb-proto
  (:use :cl :protocol-buffer)
  (:nicknames :rdp))

(defpackage :cl-rethinkdb
  (:use :cl :cl-rethinkdb-proto :cl-async-future :cl-async)
  (:export ))

(defpackage :cl-rethinkdb-reql
  (:use :cl :cl-hash-util :cl-rethinkdb :cl-rethinkdb-proto)
  (:import-from :cl-rethinkdb
                #:alistp
                #:do-list/vector)
  (:nicknames :r))

