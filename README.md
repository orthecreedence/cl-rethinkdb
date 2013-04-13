cl-rethinkdb - RethinkDB driver for Common Lisp
===============================================
This is an async [RethinkDB](http://www.rethinkdb.com/) driver for *everyone's*
favorite programming language. It does its best to follow the [query language
specification](http://www.rethinkdb.com/api/#js).

As with most of my drivers, cl-rethinkdb requires [cl-async](http://orthecreedence.github.io/cl-async/),
and makes heavy use of [cl-async's futures](http://orthecreedence.github.io/cl-async/future).

This driver is built so that later on, more than one TCP backend can be used.
Right now, the only one implemented is cl-async, but usocket/IOLib could just as
easily be used if *someone* puts in the time.

Documentation
=============
The driver makes extensive use of futures, as mentioned, so be sure to know your
way around the [future syntax macros](http://orthecreedence.github.io/cl-async/future#nicer-syntax)
when using it.

Everything needed to use the driver is exported out of the `cl-rethinkdb`
package, which has the nickname `r`.

DSL
---
cl-rethinkdb makes use of a query DSL that maps keyword function calls to normal
function calls. It does this so that predefined common lisp functions can be
used instead of giving them rediculous names to avoid naming clashes.

The DSL is activated by using either the [r](#r-macro) macro (used to build
query forms) or the [fn](#fn-macro) macro (used to build anonymous functions).

Note that this section only covers the DSL itself. Check out the [full list of
commands](#commands) to start building the query of your dreams.

### r (macro)
This macro translates keyword functions into ReQL function calls:

```common-lisp
;; grab first 10 records from the `users` table
(r (:limit (:table "users") 10))
```

This translates to
```common-lisp
(cl-rethinkdb-reql::limit (cl-rethinkdb-reql::table "users") 10)
```

### fn (macro)
This macro is very much like [r](#r-macro), and in fact wraps its inner forms
in `r` so that you can use the query DSL from within a function.

```common-lisp
;; return an anonymous function that adds `3` to the given argument
(fn (x) (:+ x 3))
```

Functions can be mixed in with `r` queries:

```common-lisp
;; find all users older than 24
(r (:filter (:table "users")
            (fn (user)
              (:< 24 (:attr user "age")))))
```

Note how inside the `fn` body, we're still using functions prefixed with `:`.

Sending queries and getting results
-----------------------------------
Once you've constructed a query via [r](#r-macro), you need to send it to the
server.

### connect (function)
```common-lisp
(defun connect (host port &key db use-outdated (read-timeout 5)))
  => future (tcp-socket)
```
Connects a socket to the given host/port and returns a future that's finished
with the socket.

Usage:
```common-lisp
(alet ((sock (connect "127.0.0.1" 28015 :db "test")))
  ;; ... do stuff ...
  (disconnect sock))
```

### run (function)
```common-lisp
(defun run (sock query-form))
  => future (atom/cursor)
```
Run a query against the given socket (connected using [connect](#connect-function)).
Returns a future finished with either the atom the query returns or a cursor to
the query results.

`run` can signal the following errors on the future it returns:

- [query-client-error](#query-client-error)
- [query-compile-error](#query-compile-error)
- [query-runtime-error](#query-runtime-error)

Example
```common-lisp
(alet* ((sock (connect "127.0.0.1" 28015))
        (query (r (:get (:table "users") 12)))  ; get user id 12
        (value (run sock query)))
  (format t "My user is: ~s~%" value)
  (disconnect sock))
```

### cursor (class)
The cursor class keeps track of queries where a sequence of results is returned
(as opposed to an atom). It is generally opaque, having no public accessors.

Cursor functions/methods:

- [cursorp](#cursorp-function)
- [next](#next-function)
- [has-next](#has-next-function)
- [to-array](#to-array-function)
- [stop](#stop-function)

### cursorp (function)
```common-lisp
(defun cursorp (cursor))
  => t/nil
```
Convenience function to tell if the given object is a cursor.

### next (function)
```common-lisp
(defun next (sock cursor))
  => future (atom)
```
Gets the next result from a cursor. Returns a future that's finished with the
next result. The result could be stored locally already, but it also may need to
be retrieved from the server.

`next` can signal two errors on the future it returns:

- [cursor-overshot](#cursor-overshot)
- [cursor-no-more-results](#cursor-no-more-results)

```common-lisp
(alet* ((sock (connect "127.0.0.1" 28015))
        (query (r (:table "users")))  ; get all users
        (cursor (run sock query)))
  ;; grab the first result from the cursor.
  (alet ((user (next sock cursor)))
    (format t "first user is: ~s~%" user)
    ;; let's grab another user
    (alet ((user (next sock cursor)))
      (format t "second user is: ~s~%" user)
      ;; let the server/driver know we're done with this result set
      (wait-for (stop cursor)
        (disconnect sock)))))
```

### has-next (function)
```common-lisp
(defun has-next (cursor))
  => t/nil
```
Determines if a cursor has more results available.

### to-array (function)
```common-lisp
(defun to-array (sock cursor))
  => future (vector)
```
Given a socket and a cursor, `to-array` grabs ALL the results from the cursor,
going out to the server to get more if it has to, and returns them as an array
through the returned future.

```common-lisp
(alet* ((sock (connect "127.0.0.1" 28015))
        (query (r (:table "users")))  ; get all users
        (cursor (run sock query))
        (all-records (to-array sock cursor)))
  (format t "All users: ~s~%" all-records)
  ;; cleanup
  (wait-for (stop sock cursor)
    (disconnect sock)))
```

### stop (function)
```common-lisp
(defun stop (sock cursor))
  => future
```
Stops a currently open query/cursor. This cleans up the cursor locally, and also
lets RethinkDB that the results for this cursor are no longer needed. Returns a
future that is finished with *no values* when the operation is complete.

### disconnect (function)
```common-lisp
(defun disconnect (sock))
  => nil
```
Disconnect a connection to a RethinkDB server.

Commands
--------
All of the following are accessible via the [r DSL macro](#r-macro) by prefixing
the name with a `:`. So `(table "users")` becomes `(:table "users")`.

These are almost 100% compatible with the [ReQL specification](http://www.rethinkdb.com/api),
so if you familiarize yourself with the query language, you will automatically
get a good handle on the following.

- db-drop (db-name) => object
- db-list () => object
- table-create (db table-name &key datacenter primary-key cache-size) => object
- table-drop (db table-name) => object
- table-list (db) => object
- insert (table sequence/object &key upsert) => object
- update (select object/function &key non-atomic) => object
- replace (select object/function &key non-atomic) => object
- delete (select) => object
- db (db-name) => db
- table (table-name) => sequence
- get (table item-id) => object
- between (sequence &key left right) => sequence
- filter (sequence object/function) => sequence
- inner-join (sequence1 sequence2 function) => sequence
- outer-join (sequence1 sequence2 function) => sequence
- eq-join (sequence1 field sequence2) => sequence
- zip (sequence) => sequence
- map (sequence function) => sequence
- concat-map (sequence function) => sequence
- order-by (sequence field &rest fields) => sequence
- asc (field) => field
- desc (field) => field
- skip (sequence number) => sequence
- limit (sequence number) => sequence
- slice (sequence start end) => sequence
- nth (sequence number) => object
- union (sequence &rest sequences) => sequence
- reduce (sequence function) => object
- count (sequence) => number
- distinct (sequence) => sequence
- grouped-map-reduce (sequence function-group function-map function-reduce) => sequence
- group-by (sequence &rest fields-then-reduction) => sequence
- count-reduce () => function
- sum-reduce (field) => function
- avg-reduce (field) => function
- attr (object field) => object
- row (&optional field) => object
- pluck (sequence/object field &rest fields) => sequence/object
- without (sequence/object field &rest fields) => sequence/object
- merge (object &rest objects) => object
- append (array object) => array
- contains (object string) => boolean
- \+ (number/string &rest numbers/strings) => number/string
- \- (number &rest numbers) => number
- \* (number &rest numbers) => number
- / (number &rest numbers) => number
- % (number mod) => number
- && (boolean &rest booleans) => boolean
- || (boolean &rest booleans) => boolean
- == (object &rest objects) => boolean
- != (object &rest objects) => boolean
- < (object &rest objects) => boolean
- <= (object &rest objects) => boolean
- > (object &rest objects) => boolean
- >= (object &rest objects) => boolean
- ~ (boolean) => boolean
- do (function &rest args) => object
- branch (boolean true-expr false-expr) => object
- foreach (sequence function) => object
- error (message) => error
- expr (lisp-object) => RethinkDB object
- js (javascript-str) => object/function
- coerce-to (object type) => object
- typeof (object) => type-string

Errors
------
These are the errors you may encounter while using this driver. Most (if not
all) errors will be signalled on a future instead of thrown directly. Errors
on a future can be caught via [future-handler-case](http://orthecreedence.github.io/cl-async/future#future-handler-case).

### query-error
A general query error.

### query-client-error
_extends [query-error](#query-error)_

Thrown when the driver sucks. If you get this, open an issue.

### query-compile-error
_extends [query-error](#query-error)_

Thrown when a query cannot compile. If you get this, take a close look at your
query forms.

### query-runtime-error
_extends [query-error](#query-error)_

Thrown when the database has a runtime error.

### cursor-error
A general error with a cursor.

### cursor-overshot
_extends [cursor-error](#cursor-error)_

Thrown when [next](#next-function) is called on a cursor, but the cursor is
currently grabbing more results.

### cursor-no-more-results
_extends [cursor-error](#cursor-error)_

Thrown when [next](#next-function) is called on a cursor that has no more
results. You can test this by using [has-next](#has-next-function).

License
-------
MIT. Enjoy.

