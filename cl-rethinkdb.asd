(asdf:defsystem cl-rethinkdb
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.6.0"
  :description "A RethinkDB driver for Common Lisp"
  :depends-on (#:blackbird
               #:vom
               #:cl-async
               #:fast-io
               #:yason
               #:cl-base64
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
     ((:file "types")
      (:file "function")
      (:file "commands")
      (:file "dsl")
      (:file "pseudotypes"))
     :depends-on ("config"))
   (:file "connection" :depends-on (reql "config"))
   (:file "query" :depends-on (reql "protocol" "config" "connection"))))

