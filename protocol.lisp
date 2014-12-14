(in-package :cl-rethinkdb)

(defconstant +proto-version+ #x5f75e83e)
(defconstant +proto-json+ #x7e6970c7)

(defconstant +proto-query-start+ 1)
(defconstant +proto-query-continue+ 2)
(defconstant +proto-query-stop+ 3)
(defconstant +proto-query-wait+ 4)

(defconstant +rdb-response-atom+ 1)
(defconstant +rdb-response-sequence+ 2)
(defconstant +rdb-response-partial+ 3)
(defconstant +rdb-response-feed+ 5)
(defconstant +rdb-response-wait-complete+ 4)
(defconstant +rdb-response-client-error+ 16)
(defconstant +rdb-response-compile-error+ 17)
(defconstant +rdb-response-runtime-error+ 18)

(defconstant +datum-type-null+ 1)
(defconstant +datum-type-bool+ 2)
(defconstant +datum-type-num+ 3)
(defconstant +datum-type-str+ 4)
(defconstant +datum-type-array+ 5)
(defconstant +datum-type-object+ 6)
(defconstant +datum-type-json+ 7)

