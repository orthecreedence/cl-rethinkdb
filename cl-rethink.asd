(asdf:defsystem cl-rethink
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.0.1"
  :description "A RethinkDB driver for Common Lisp"
  :depends-on (#:protobuf #:cl-async-future)
  :components
  ((:file "package")
   (:file "protocol" :depends-on ("package"))

   ))

