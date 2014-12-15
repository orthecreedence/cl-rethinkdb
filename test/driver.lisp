(defpackage :cl-rethinkdb-test
  (:use :cl :fiveam :blackbird :cl-rethinkdb :cl-hash-util)
  (:shadow cl-rethinkdb:run)
  (:export #:run-tests))
(in-package :cl-rethinkdb-test)
(def-suite cl-rethinkdb-test :description "cl-rethinkdb test suite")
(in-suite cl-rethinkdb-test)

(defmacro setup (bindings error-bind test-body &body check-body)
  "Makes setting up standard tests easier."
  `(let ,(append bindings (list error-bind))
     (as:with-event-loop ()
       (catcher
         ,test-body
         (t (e) (setf ,error-bind e))))
     ,@check-body))

(defun conn ()
  (connect "127.0.0.1" 28015 :db "test" :read-timeout 10))

(defun json (obj)
  "Guess."
  (with-output-to-string (s)
    (yason:encode obj s)))

(test query-lang
  "Test that creation of queries works"
  (let ((q1 (r:r (:table "test")))
        (q2 (r:r (:+ 3 8)))
        (q3 (r:fn (x) (:get (:table "users") x))))
    (is (equalp q1 '(15 ("test"))))
    (is (equalp q2 '(24 (3 8))))
    (is (eq (car q3) 69))))

(test (connect :depends-on query-lang)
  "Test connections"
  (setup ((the-sock nil)) err
    (alet* ((sock (conn)))
      (setf the-sock sock)
      (disconnect sock))
    (is (eq err nil))
    (is (typep the-sock 'as:socket))))

(test (reset :depends-on connect)
  "Clear out old test stuff."
  (setup ((res nil)
          sock) err
    (chain (conn)
      (:then (socket)
        (setf sock socket)
        (r:run sock (r:r (:table-drop "users"))))
      (:then (qres &rest _)
        (declare (ignore _))
        (setf res qres))
      (:catch (e)
        (setf err e))
      (:finally
        (disconnect sock)))
    (is (or res err))))
      
(test (setup :depends-on reset)
  "Test setting up a table for testing"
  (setup ((res nil)
          sock) err
    (chain (conn)
      (:then (socket)
        (setf sock socket)
        (r:run sock (r:r (:table-create "users"))))
      (:then (qres &rest _)
        (declare (ignore _))
        (setf res qres))
      (:finally
        (disconnect sock)))
    (is (string= (json res) "{\"created\":1}"))
    (is (eq err nil))))

(setf *debug-on-error* t)

(r:r (:insert (:table "users") (list (hash ("name" "andrew")
                                           ("age" 28)
                                           ("pets" '("timmy" "wookie" "lucy")))
                                     (hash ("name" "larry")
                                           ("age" 52)
                                           ("pets" '("ricky raccoon" "jack (in the pulpit)")))
                                     (hash ("name" "slappy")
                                           ("age" 23)
                                           ("pets" '("barry"))))))
(test (insert :depends-on setup)
  "Test inserts"
  (format t "~%---~%")
  (setup ((res nil)
          (users (list (hash ("name" "andrew")
                             ("age" 28)
                             ("pets" '("timmy" "wookie" "lucy")))
                       (hash ("name" "larry")
                             ("age" 52)
                             ("pets" '("ricky raccoon" "jack (in the pulpit)")))
                       (hash ("name" "slappy")
                             ("age" 23)
                             ("pets" '("barry")))))
          sock) err
    (chain (conn)
      (:then (socket)
        (setf sock socket)
        (let ((query (r:r (:insert (:table "users") users))))
          (format t "~%---~%query: ~s~%" query)
          (r:run sock query)))
      (:then (qres &rest _)
        (declare (ignore _))
        (format t "~%---~%res: ~a~%" qres)
        (setf res qres))
      (:catch (e) (setf err e))
      (:finally (disconnect sock)))
    (is (string= 
          (json (mapcar (lambda (x) (remhash "generated_keys" x)) res))
          "{\"unchanged\":0,\"deleted\":0,\"inserted\":1,\"errors\":0,\"skipped\":0,\"replaced\":0}"))
    (format t "~%---~%err: ~a~%" err)
    (is (eq err nil))))
