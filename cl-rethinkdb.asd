(asdf:defsystem cl-rethinkdb
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.0.2"
  :description "A RethinkDB driver for Common Lisp"
  :depends-on (#:protobuf #:parenscript #:cl-async-future #:cl-async)
  :components
  ((:file "util")
   (:file "package" :depends-on ("util"))
   (:file "protocol" :depends-on ("package"))
   (:file "query" :depends-on ("protocol"))
   (:file "connection" :depends-on ("query"))
   (:module reql
    :serial t
	:components
	((:file "package")
	 (:file "datum")
	 (:file "term")
	 (:file "javascript")
	 (:file "commands"))
	:depends-on ("protocol"))))

