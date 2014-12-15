(asdf:defsystem cl-rethinkdb-test
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.2"
  :description "TESTS FOR cl-rethinkdb."
  :depends-on (#:cl-async
               #:blackbird
               #:fiveam
               #:cl-rethinkdb
               #:cl-ppcre)
  :components
  ((:module test
    :serial t
    :components ((:file "util")
                 (:file "driver")
                 (:file "run")))))

