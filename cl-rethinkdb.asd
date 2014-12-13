(asdf:defsystem cl-rethinkdb
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.5.0"
  :description "A RethinkDB driver for Common Lisp"
  :depends-on (#:cl-async-future
               #:cl-async
               #:flexi-streams
               #:cl-hash-util
               #:cl-ppcre)
  :components
  ((:file "util")
   (:file "package" :depends-on ("util"))
   (:file "config" :depends-on ("package"))
   (:file "protocol" :depends-on ("package"))
   (:module reql
     :serial t
     :components
     ((:file "function")
      (:file "commands")
      (:file "dsl"))
     :depends-on ("config"))
   (:file "connection" :depends-on (reql "config"))
   (:file "query" :depends-on (reql "protocol" "config" "connection"))))

