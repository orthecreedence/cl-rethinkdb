cl-rethinkdb - RethinkDB driver for Common Lisp
===============================================
This is an async [RethinkDB](http://www.rethinkdb.com/) driver for *everyone's*
favorite programming language. It does its best to follow the [query language
specification](http://www.rethinkdb.com/api/#js).

As with most of my drivers, cl-rethinkdb requires [cl-async](http://orthecreedence.github.io/cl-async/),
and makes heavy use of [cl-async's futures](http://orthecreedence.github.io/cl-async/future).

__!!! This driver is unfinished !!!__ Also, the query language may change in
newer versions of RethinkDB and break the driver. I'll do my best to stay on top
of changes but I only have five arms.

Documentation
=============
At the moment, I'm too lazy to document all the commands, so for now you'll
have to get by with some examples.

JS:
```javascript
var conn = r.connect({host: 'localhost', port: 28015});
r.table('users').get(1234).run(conn);
```

Lisp:
```common-lisp
(alet* ((conn (cl-rethinkdb::connect "localhost" 28015))
        (query (cl-rethinkdb-reql:r
                 (:get (:table "users") 1234)))
        (result (cl-rethinkdb::run conn query)))
  (format t "Result: ~s~%" result))
```

The `cl-rethinkdb-reql:r` macro provides a makeshift DSL for constructing
queries. It does this instead of using a direct export because the query
language defines a lot of pre-defined common lisp functions which, if
imported to an application, could screw things up quite a bit.
