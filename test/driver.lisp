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
  (jonathan:to-json obj))

(test query-lang
  "Test that creation/serialization of queries works"
  (let ((q1 (json (r:r (:table "test"))))
        (q2 (json (r:r (:+ 3 8))))
        (q3 (json (r:fn (x) (:get (:table "users") x)))))
    (is (string= q1 "[15,[\"test\"]]"))
    (is (string= q2 "[24,[3,8]]"))
    (is (cl-ppcre:scan "\\[69,\\[\\[2,\\[[0-9]\+\\]\\],\\[16,\\[\\[15,\\[\"users\"\\]\\],\\[10,\\[[0-9]\+\\]\\]\\]\\]\\]\\]"
                       q3))))

(test (connect :depends-on query-lang)
  "Test connections"
  (setup ((sock nil)) err
    (chain (conn)
      (:then (the-sock)
        (setf sock the-sock))
      (:catch (e) (setf err e))
      (:finally
        (when sock
          (disconnect sock))))
    (is (eq err nil))
    (is (typep sock 'as:socket))))

(test (reset :depends-on connect)
  "Clear out old test stuff."
  (setup ((res nil)
          sock) err
    (chain (conn)
      (:then (socket)
        (setf sock socket)
        (r:run sock (r:r (:table-drop "users"))))
      (:then (qres)
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
      (:then (qres)
        (setf res qres))
      (:catch (e)
        (setf err e))
      (:finally
        (disconnect sock)))
    (is (string= (json res) "{\"created\":1}"))
    (is (eq err nil))))

(test (insert :depends-on setup)
  "Test (multi) inserts"
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
          (r:run sock query)))
      (:then (qres)
        (setf res qres))
      (:catch (e) (setf err e))
      (:finally (disconnect sock)))
    (remhash "generated_keys" res)
    (is (string= 
          (json res)
          "{\"unchanged\":0,\"deleted\":0,\"inserted\":3,\"errors\":0,\"skipped\":0,\"replaced\":0}"))
    (is (eq err nil))))

(test (filter :depends-on insert)
  "Test that filtering/functions work properly"
  (setup (res sock cur) err
    (chain (conn)
      (:then (socket)
        (setf sock socket)
        (r:run sock (r:r (:filter
                           (:table "users")
                           (r:fn (x) (:== (:attr x "name") "slappy"))))))
      (:then (cursor _)
        (declare (ignore _))
        (setf cur cursor)
        (next sock cursor))
      (:then (val)
        (setf res val))
      (:catch (e)
        (setf err e))
      (:finally
        (stop/disconnect sock cur)))
    (is (eq (gethash "age" res) 23))
    (is (eq err nil))))

(test (delete :depends-on filter)
  "Test deletes"
  (setup (res sock) err
    (chain (conn)
      (:then (socket)
        (setf sock socket)
        (r:run sock (r:r (:delete
                           (:filter
                             (:table "users")
                             (r:fn (x) (:== (:attr x "age") 23)))))))
      (:then (qres _)
        (declare (ignore _))
        (setf res qres))
      (:catch (e)
        (setf err e))
      (:finally
        (disconnect sock)))
    (is (eq (gethash "deleted" res) 1))
    (is (eq err nil))))

(test basic-ops
  "Test some basic stuff"
  (setup (res1 res2 res3 sock) err
    (chain (conn)
      (:then (socket)
        (setf sock socket)
        (r:run sock (r:r (:+ 4 5))))
      (:then (res)
        (setf res1 res)
        (r:run sock (r:r (:do (r:fn (x y) (:* x y))
                              6 9))))
      (:then (res)
        (setf res2 res)
        (r:run sock (r:r (:match "i am a troll" "i.*troll"))))
      (:then (res)
        (setf res3 res))
      (:catch (e)
        (setf err e))
      (:finally
        (disconnect sock)))
    (is (eq res1 9))
    (is (eq res2 54))
    (is (eq (gethash "start" res3) 0))
    (is (eq (gethash "end" res3) 12))))

