cl-rethinkdb - RethinkDB driver for Common Lisp
===============================================
This is an async [RethinkDB](http://www.rethinkdb.com/) driver for *everyone's*
favorite programming language. It does its best to follow the [query language
specification](http://www.rethinkdb.com/api/#js).

*This driver is up to date with RethinkDB's v1.7 protocol.*

As with most of my drivers, cl-rethinkdb requires [cl-async](http://orthecreedence.github.io/cl-async/),
and makes heavy use of [cl-async's futures](http://orthecreedence.github.io/cl-async/future).

This driver is built so that later on, more than one TCP backend can be used.
Right now, the only one implemented is cl-async, but usocket/IOLib could just as
easily be used if *someone* puts in the time.

Type issues
===========
This driver has been working great for me for some time now. The biggest issue I
run into while using it is type mismatches.

For instance, I'll construct a perfectly valid query, and the driver will whine
about how an assert failed while checking one of the types of the arguments in
the query.

If this happens, please open an issue and I'll do my best to fix it in a few
hours. I do my best to match the type hierarchy from the query specification to
the internal types used, but even *I* make mistakes sometimes.

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
This macro creates an anonymous function for use in a RethinkDB query.

It works very much like the [r macro](#r-macro), and in fact wraps its inner
forms in `r` so that you can use the query DSL from within a function.

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
server. When the server responds successfully, you will get either an atom (a
single value: integer, boolean, hash, array, etc). or a [cursor](#cursor-class)
which provides an interface to iterate over a set of atoms.

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
- [each](#each-function)
- [stop](#stop-function)
- [stop/disconnect](#stop-disconnect-function)

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

### each (function)
```common-lisp
(defun each (sock cursor function))
  => future 
```
Call the given function on each of the results of a cursor. The returned future
is finished when all results have been iterated over.

```common-lisp
(alet* ((sock (connect "127.0.0.1" 28015))
        (cursor (run sock (r (:table "users")))))
  ;; print each user
  (wait-for (each sock cursor
              (lambda (x) (format t "user: ~s~%" x)))
    ;; cleanup
    (wait-for (stop sock cursor)
      (disconnect sock))))
```

### stop (function)
```common-lisp
(defun stop (sock cursor))
  => future
```
Stops a currently open query/cursor. This cleans up the cursor locally, and also
lets RethinkDB know that the results for this cursor are no longer needed.
Returns a future that is finished with *no values* when the operation is
complete.

### stop/disconnect (function)
```common-lisp
(defun stop/disconnect (sock cursor))
  => nil
```
Calls [stop](#stop-function) on a cursor, and after the stop operation is done
closes the passed socket. Useful as a final termination to an operation that
uses a cursor.

Note that this function checks if the object passed is indeed a cursor, and if
not, just disconnects the socket without throwing any errors.

### disconnect (function)
```common-lisp
(defun disconnect (sock))
  => nil
```
Disconnect a connection to a RethinkDB server. 

Config
------
These mainly have to do with how you want data returned.

### \*sequence-type\*
When a sequence is returned from RethinkDB, it can be either returned as a list
(if `*sequence-type*` is `:list` or as a vector (if `*sequence-type*` is
`:array`). It's really a matter of preference on how you're going to access the
data.

Default: `:list`

### \*object-type\*
If an object (as in, key/value object) is returned from RethinkDB, it can be
encoded as a hash table (if `*object-type*` is `:hash`) or as an association
list (if `*object-type*` is `:alist`). Hash tables are almost always more
performant, but alists can be easier to debug. Your choice.

Default: `:hash`

Commands
--------
All of the following are accessible via the [r DSL macro](#r-macro) by prefixing
the name with a `:`. So `(table "users")` becomes `(:table "users")`.

These are almost 100% compatible with the [ReQL specification](http://www.rethinkdb.com/api),
so if you familiarize yourself with the query language, you will automatically
get a good handle on the following.

For a better understanding of the return types of the following commands, see
[the REQL type hierarchy in the protobuf specification](https://github.com/rethinkdb/rethinkdb/blob/next/src/rdb_protocol/ql2.proto).

- `db-drop (db-name) => object`
- `db-list () => object`
- `table-create (db table-name &key datacenter primary-key cache-size durability) => object`
- `table-drop (db table-name) => object`
- `table-list (db) => object`
- `index-create (table name &optional reql-function) => object`
- `index-drop (table name) => object`
- `index-list (table) => array`
- `insert (table sequence/object &key upsert durability return-vals) => object`
- `update (select object/function &key non-atomic durability return-vals) => object`
- `replace (select object/function &key non-atomic durability return-vals) => object`
- `delete (select &key durability return-vals) => object`
- `db (db-name) => db`
- `table (table-name) => sequence`
- `get (table item-id) => object`
- `get-all (table key/keys &key index) => array`  
  (`key/keys` can be either a string type or a list of string types)
- `between (sequence &key left right index) => sequence`
- `filter (sequence object/function &key default) => sequence`
- `inner-join (sequence1 sequence2 function) => sequence`
- `outer-join (sequence1 sequence2 function) => sequence`
- `eq-join (sequence1 field sequence2 &key index) => sequence`
- `zip (sequence) => sequence`
- `map (sequence function) => sequence`
- `with-fields (sequence &rest strings) => sequence`
- `concat-map (sequence function) => sequence`
- `order-by (sequence field &rest fields) => sequence`
- `asc (field) => field`
- `desc (field) => field`
- `skip (sequence number) => sequence`
- `limit (sequence number) => sequence`
- `slice (sequence start end) => sequence`
- `nth (sequence number) => object`
- `indexes-of (sequence object/reql-function) => sequence`
- `is-empty (sequence) => boolean`
- `union (sequence &rest sequences) => sequence`
- `sample (sequence count) => sequence`
- `reduce (sequence function) => object`
- `count (sequence &optional object/reql-function) => number`
- `distinct (sequence) => sequence`
- `grouped-map-reduce (sequence function-group function-map function-reduce) => sequence`
- `group-by (sequence &rest fields-then-reduction) => sequence`
- `contains (sequence object) => boolean`
- `count-reduce () => function`
- `sum-reduce (field) => function`
- `avg-reduce (field) => function`
- `attr (object field) => object`
- `row (&optional field) => object`
- `pluck (sequence/object field &rest fields) => sequence/object`
- `without (sequence/object field &rest fields) => sequence/object`
- `merge (object &rest objects) => object`
- `append (array object) => array`
- `prepend (array object) => array`
- `difference (array1 array2) => array`
- `set-insert (array object) => array`
- `set-intersection (array1 array2) => array`
- `set-union (array1 array2) => array`
- `set-difference (array1 array2) => array`
- `has-fields (object string &rest strings) => bool`
- `insert-at (array index object) => array`
- `splice-at (array1 index array2) => array`
- `delete-at (array index) => array`
- `change-at (array index object) => array`
- `keys (object) => array`
- `\+ (number/string &rest numbers/strings) => number/string`
- `\- (number &rest numbers) => number`
- `\* (number &rest numbers) => number`
- `/ (number &rest numbers) => number`
- `% (number mod) => number`
- `&& (boolean &rest booleans) => boolean`
- `|| (boolean &rest booleans) => boolean`
- `== (object &rest objects) => boolean`
- `!= (object &rest objects) => boolean`
- `< (object &rest objects) => boolean`
- `<= (object &rest objects) => boolean`
- `> (object &rest objects) => boolean`
- `>= (object &rest objects) => boolean`
- `~ (boolean) => boolean`
- `match (string string-regex) => object`
- `do (function &rest args) => object`
- `branch (boolean true-expr false-expr) => object`
- `foreach (sequence function) => object`
- `error (message) => error`
- `default (top1 top2) => top`
- `expr (lisp-object) => RethinkDB object`
- `js (javascript-str) => object/function`
- `coerce-to (object type) => object`
- `typeof (object) => type-string`
- `info (object) => object`
- `json (string) => object`
- `literal (object) => object`

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

