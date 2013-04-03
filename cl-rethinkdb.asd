(asdf:defsystem cl-rethinkdb
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.0.1"
  :description "A RethinkDB driver for Common Lisp"
  :depends-on (#:protobuf #:cl-hash-util #:cl-async-future #:cl-async)
  :components
  ((:file "package")
   (:file "util" :depends-on ("package"))
   (:file "protocol" :depends-on ("util"))
   (:module reql
    :serial t
	:components
	((:file "datum")
	 (:file "term")
	 (:file "commands"))
	:depends-on ("util"))
   (:file "connection" :depends-on ("util"))))

