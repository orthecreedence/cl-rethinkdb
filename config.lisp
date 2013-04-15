(in-package :cl-rethinkdb)

(defvar *sequence-type* :list
  "Defines whether data returned as a sequence should be a :list or :array.")

(defvar *object-type* :hash
  "Defines whether data returned as an object should be an :aslist or :hash.")
