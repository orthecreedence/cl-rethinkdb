(asdf:defsystem cl-rethinkdb
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.4.0"
  :description "A RethinkDB driver for Common Lisp"
  :depends-on (#:protobuf #:cl-async-future #:cl-async)
  :components
  ((:file "util")
   (:file "package" :depends-on ("util"))
   (:file "protocol" :depends-on ("package"))
   (:file "config" :depends-on ("package"))
   (:module reql
    :serial t
	:components
	((:file "import")
	 (:file "types")
	 (:file "datum")
	 (:file "term")
	 (:file "function")
	 (:file "commands")
	 (:file "dsl"))
	:depends-on ("protocol" "config"))
   (:file "connection" :depends-on (reql "config"))
   (:file "query" :depends-on (reql "protocol" "config" "connection"))))

