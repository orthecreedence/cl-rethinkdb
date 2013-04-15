(in-package :cl-rethinkdb-reql)

;; -----------------------------------------------------------------------------
;; utils
;; -----------------------------------------------------------------------------
(defun wrap-in-term (object)
  "Make sure a sequence type is wrapped in a term."
  (cond ((typep object 'term)
         object)
        (t
         (term-from-datum (create-datum object)))))

(defmacro assert-fn-args (reql-function num-args)
  "Assert that a function has the correct number of arguments."
  (let ((fn (gensym "fn")))
    `(let ((,fn ,reql-function))
       (when (is-term +term-term-type-func+ ,fn)
         (assert (eq (num-args ,fn) ,num-args))))))

(defparameter *commands* nil
  "Holds all commands defined with defcommand.")

(defmacro defcommand (name arglist &body body)
  "Simple wrapper for defining commands that pushes the command name into the
   command registry, used by the DSL."
  `(progn
     (cl:delete ',name *commands*)
     (defun ,name ,arglist ,@body)
     (push ',name *commands*)
     ',name))

;; -----------------------------------------------------------------------------
;; manipulating databases
;; -----------------------------------------------------------------------------
(defcommand db-create (db-name)
  "Create a DB."
  (assert (is-string db-name))
  (create-term +term-term-type-db-create+ (list (wrap-in-term db-name))))
  
(defcommand db-drop (db-name)
  "Drop a DB."
  (assert (is-string db-name))
  (create-term +term-term-type-db-drop+ (list (wrap-in-term db-name))))
  
(defcommand db-list ()
  "List DBs."
  (create-term +term-term-type-db-list+))

;; -----------------------------------------------------------------------------
;; manipulating tables
;; -----------------------------------------------------------------------------
(defcommand table-create (database table-name &key datacenter primary-key cache-size)
  "Create a table in the given database, optionally specifying the datacenter,
   primary key of the table (default 'id') and table cache size."
  (assert (is-term +term-term-type-db+ database))
  (assert (is-string table-name))
  (assert (or (null datacenter)
              (is-string datacenter)))
  (assert (or (null primary-key)
              (is-string primary-key)))
  (assert (or (null cache-size)
              (is-number cache-size)))
  (let ((options nil))
    (when datacenter (push (cons "datacanter" datacenter) options))
    (when primary-key (push (cons "primary_key" primary-key) options))
    (when cache-size (push (cons "cache_size" cache-size) options))
    (create-term +term-term-type-table-create+
                 (list (wrap-in-term database)
                       (wrap-in-term table-name))
                 options)))
    
(defcommand table-drop (database table-name)
  "Drop a table from a database."
  (assert (is-term +term-term-type-db+ database))
  (assert (is-string table-name))
  (create-term +term-term-type-table-drop+
               (list (wrap-in-term database)
                     (wrap-in-term table-name))))

(defcommand table-list (database)
  "List tables in a database."
  (assert (is-term +term-term-type-db+ database))
  (create-term +term-term-type-table-list+ (list (wrap-in-term database))))

;; -----------------------------------------------------------------------------
;; writing data
;; -----------------------------------------------------------------------------
(defcommand insert (table sequence/object &key upsert)
  "Create an insert query, given a table object and a set of values.

   The value can be a hash table (or alist), or an array of hashes/alists (in
   the case of a multi-insert)."
  (assert (is-term +term-term-type-table+ table))
  (assert (or (is-sequence sequence/object)
              (is-object sequence/object)))
  (assert (is-boolean upsert))
  (create-term +term-term-type-insert+
               (list (wrap-in-term table)
                     (wrap-in-term sequence/object))
               (when upsert '(("upsert" . t)))))

(defcommand update (select object/reql-function &key non-atomic)
  "Update an object or set of objects (a select) using the given object or REQL
   function object. Supports using non-atomic writing via :non-atomic."
  (assert (is-select select))
  (assert (or (is-object object/reql-function)
              (is-function object/reql-function)))
  (assert-fn-args object/reql-function 2)
  (assert (is-boolean non-atomic))
  (create-term +term-term-type-update+
               (list (wrap-in-term select)
                     (wrap-in-term object/reql-function))
               (when non-atomic
                 '(("non_atomic" . t)))))

(defcommand replace (select object/reql-function &key non-atomic)
  "Replace an entire object or set of objects (a select) using the given object
   REQL function string. Supports using non-atomic writing via :non-atomic.
   
   The replacement object needs to have the primary key in it."
  (assert (is-select select))
  (assert (or (is-object object/reql-function)
              (is-function object/reql-function)))
  (assert-fn-args object/reql-function 2)
  (assert (is-boolean non-atomic))
  (create-term +term-term-type-replace+
               (list (wrap-in-term select)
                     (wrap-in-term object/reql-function))
               (when non-atomic
                 '(("non_atomic" . t)))))

(defcommand delete (select)
  "Delete an object or set of objects (a select)."
  (assert (is-select select))
  (create-term +term-term-type-delete+ (list (wrap-in-term select))))

;; -----------------------------------------------------------------------------
;; selecting data
;; -----------------------------------------------------------------------------
(defcommand db (db-name)
  "Create a database object."
  (assert (is-string db-name))
  (create-term +term-term-type-db+ (list (wrap-in-term db-name))))

(defcommand table (table-name &key db use-outdated)
  "Create a table object. Can optionally specify which database the table
   belongs to via :db.
   
   :use-outdated allows you to specify that out-of-date data is ok when querying
   this table."
  (assert (is-string table-name))
  (assert (or (null db)
              (is-term +term-term-type-db+ db)))
  (assert (is-boolean use-outdated))
  (let ((args (list (wrap-in-term table-name))))
    (when db (push (wrap-in-term db) args))
    (create-term +term-term-type-table+
                 args
                 (when use-outdated `(("use_outdated" . ,use-outdated))))))

(defcommand get (table id)
  "Creates a query to grab an object with the given ID (string or int) from the
   given table."
  (assert (is-term +term-term-type-table+ table))
  (assert (is-pkey id))
  (create-term +term-term-type-get+
               (list (wrap-in-term table)
                     (wrap-in-term id))))

(defcommand between (select &key left right)
  "Grabs objects from a selection where the primary keys are between two values."
  (assert (is-select select))
  (assert (or (null left)
              (is-pkey left)))
  (assert (or (null right)
              (is-pkey right)))
  (let ((options nil))
    (when left (push (cons "left_bound" left) options))
    (when right (push (cons "right_bound" right) options))
    (create-term +term-term-type-between+
                 (list (wrap-in-term select))
                 options)))

(defcommand filter (sequence object/reql-function)
  "Filter a sequence by either an object or a REQL function."
  (assert (is-sequence sequence))
  (assert (or (is-object object/reql-function)
              (is-function object/reql-function)))
  (create-term +term-term-type-filter+
               (list (wrap-in-term sequence)
                     (wrap-in-term object/reql-function))))

;; -----------------------------------------------------------------------------
;; joins
;; -----------------------------------------------------------------------------
(defcommand inner-join (sequence1 sequence2 reql-function)
  "Perform an inner join on two sequences using the given REQL function."
  (assert (is-sequence sequence1))
  (assert (is-sequence sequence2))
  (assert (is-function reql-function))
  (assert-fn-args reql-function 2)
  (create-term +term-term-type-inner-join+
               (list (wrap-in-term sequence1)
                     (wrap-in-term sequence2)
                     (wrap-in-term reql-function))))
  
(defcommand outer-join (sequence1 sequence2 reql-function)
  "Perform a left outer join on two sequences using the given REQL function."
  (assert (is-sequence sequence1))
  (assert (is-sequence sequence2))
  (assert (is-function reql-function))
  (assert-fn-args reql-function 2)
  (create-term +term-term-type-outer-join+
               (list (wrap-in-term sequence1)
                     (wrap-in-term sequence2)
                     (wrap-in-term reql-function))))
  
(defcommand eq-join (sequence1 field sequence2)
  "Perform an equality join on two sequences by the given attribute name."
  (assert (is-sequence sequence1))
  (assert (stringp field))
  (assert (is-sequence sequence2))
  (create-term +term-term-type-eq-join+
               (list (wrap-in-term sequence1)
                     (wrap-in-term field)
                     (wrap-in-term sequence2))))

(defcommand zip (sequence)
  "Merge left/right fields of each member of a join."
  (assert (is-sequence sequence))
  (create-term +term-term-type-zip+ (list (wrap-in-term sequence))))

;; -----------------------------------------------------------------------------
;; transformations
;; -----------------------------------------------------------------------------
(defcommand map (sequence reql-function)
  "Perform a map (as in map/reduce) on a sequence."
  (assert (is-sequence sequence))
  (assert (is-function reql-function))
  (assert-fn-args reql-function 1)
  (create-term +term-term-type-map+
               (list (wrap-in-term sequence)
                     (wrap-in-term reql-function))))

(defcommand concat-map (sequence reql-function)
  "Construct a sequence of all elements returned by the given mapping function."
  (assert (is-sequence sequence))
  (assert (is-function reql-function))
  (assert-fn-args reql-function 1)
  (create-term +term-term-type-concatmap+
               (list (wrap-in-term sequence)
                     (wrap-in-term reql-function))))

(defcommand order-by (sequence field &rest fields)
  "Order a sequence by fields."
  (assert (is-sequence sequence))
  (push field fields)
  (dolist (field fields)
    (assert (or (stringp field)
                (is-term (list +term-term-type-asc+
                               +term-term-type-desc+)
                         field))))
  (create-term +term-term-type-orderby+
               (cl:append (list (wrap-in-term sequence))
                          (loop for f in fields
                                collect (wrap-in-term f)))))

(defcommand asc (field)
  "Used in ordre-by queries to specify a field is ascending in order."
  (assert (is-string field))
  (create-term +term-term-type-asc+
               (list (wrap-in-term field))))

(defcommand desc (field)
  "Used in ordre-by queries to specify a field is descending in order."
  (assert (is-string field))
  (create-term +term-term-type-desc+
               (list (wrap-in-term field))))

(defcommand skip (sequence number)
  "Skip a number of items in a sequence."
  (assert (is-sequence sequence))
  (assert (is-number number))
  (create-term +term-term-type-skip+
               (list (wrap-in-term sequence)
                     (wrap-in-term number))))

(defcommand limit (sequence number)
  "Limit a sequence by a number."
  (assert (is-sequence sequence))
  (assert (is-number number))
  (create-term +term-term-type-limit+
               (list (wrap-in-term sequence)
                     (wrap-in-term number))))

(defcommand slice (sequence start end)
  "Slice a sequence by a start and end index value."
  (assert (is-sequence sequence))
  (assert (is-number start))
  (assert (is-number end))
  (create-term +term-term-type-slice+
               (list (wrap-in-term sequence)
                     (wrap-in-term start)
                     (wrap-in-term end))))

(defcommand nth (sequence number)
  "Get the nth element of a sequence."
  (assert (is-sequence sequence))
  (assert (is-number number))
  (create-term +term-term-type-nth+
               (list (wrap-in-term sequence)
                     (wrap-in-term number))))

(defcommand union (sequence &rest sequences)
  "Perform a union on a number of sequences."
  (push sequence sequences)
  (dolist (seq sequences)
    (assert (is-sequence seq)))
  (create-term +term-term-type-union+
               (loop for s in sequences
                     collect (wrap-in-term s))))

;; -----------------------------------------------------------------------------
;; aggregation
;; -----------------------------------------------------------------------------
(defcommand reduce (sequence reql-function)
  "Perform a reduce on sequence using the given REQL function."
  (assert (is-sequence sequence))
  (assert (is-function reql-function))
  (assert-fn-args reql-function 2)
  (create-term +term-term-type-reduce+
               (list (wrap-in-term sequence)
                     (wrap-in-term reql-function))))

(defcommand count (sequence)
  "Counts the items in a sequence."
  (assert (is-sequence sequence))
  (create-term +term-term-type-count+
               (list (wrap-in-term sequence))))

(defcommand distinct (sequence)
  "Get all the distinct elements in a sequence (ie remove-duplicates)."
  (assert (is-sequence sequence))
  (create-term +term-term-type-distinct+
               (list (wrap-in-term sequence))))

(defcommand grouped-map-reduce (sequence function-group function-map function-reduce)
  "Partition a sequence into groups then perform map/reduce on those groups."
  (assert (is-sequence sequence))
  (assert (is-function function-group))
  (assert (is-function function-map))
  (assert (is-function function-reduce))
  (assert-fn-args function-group 1)
  (assert-fn-args function-map 1)
  (assert-fn-args function-reduce 2)
  (create-term +term-term-type-grouped-map-reduce+
               (list (wrap-in-term function-group)
                     (wrap-in-term function-map)
                     (wrap-in-term function-reduce))))

(defcommand group-by (sequence &rest fields-then-reduction)
  "Group elements in a sequence by the values of the given fields and the
   reduction function (always the last value)."
  (let ((reduce-fn (car (last fields-then-reduction)))
        (fields (butlast fields-then-reduction)))
    (assert (is-sequence sequence))
    (dolist (field fields)
      (assert (stringp field)))
    (assert (is-function reduce-fn))
    (create-term +term-term-type-groupby+
                 (list (wrap-in-term sequence)
                       (term-array fields)
                       (wrap-in-term reduce-fn)))))

;; -----------------------------------------------------------------------------
;; reductions
;; -----------------------------------------------------------------------------
(defcommand count-reduce ()
  "A count reduction object."
  (term-object '(("COUNT" . t))))

(defcommand sum-reduce (field)
  "Sum a field as a reduction."
  (term-object `(("SUM" . ,field))))

(defcommand avg-reduce (field)
  "Average a field as a reduction."
  (term-object `(("AVG" . ,field))))

;; -----------------------------------------------------------------------------
;; document manipulation
;; -----------------------------------------------------------------------------
(defcommand attr (object field)
  "Grab an object attribute from an object. Can be nested:

     (attr \"name\" (attr \"user\" (row)))

   Would be r.row(\"user\")(\"name\") in JS."
  (assert (is-object object))
  (assert (is-string field))
  (create-term +term-term-type-getattr+
               (list (wrap-in-term object)
                     (wrap-in-term field))))

(defcommand row (&optional field)
  "Return a reference to the current row. Optionally takes a string field, in
   which case it pulls thhat field from the row:

     (row \"age\")

   Which is a quicker way of saying:

     (attr \"age\" (row))"
  (assert (or (null field)
              (is-string field)))
  (let ((row (create-term +term-term-type-implicit-var+)))
    (if field
        (attr row field)
        row)))

(defcommand pluck (sequence/object field &rest fields)
  "Given a sequence or object, return a sequence or object with only the given
   field names present."
  (assert (or (is-object sequence/object)
              (is-sequence sequence/object)))
  (push field fields)
  (dolist (field fields)
    (assert (is-string field)))
  (create-term +term-term-type-pluck+
               (cl:append (list (wrap-in-term sequence/object))
                          (loop for f in fields
                                collect (wrap-in-term f)))))

(defcommand without (sequence/object field &rest fields)
  "Given a sequence or object, return a sequence or object without the given
   field names present."
  (assert (or (is-object sequence/object)
              (is-sequence sequence/object)))
  (push field fields)
  (dolist (field fields)
    (assert (is-string field)))
  (create-term +term-term-type-without+
               (cl:append (list (wrap-in-term sequence/object))
                          (loop for f in fields
                                collect (wrap-in-term f)))))

(defcommand merge (object &rest objects)
  "Merge objects together (merge their fields into one object)."
  (push object objects)
  (dolist (object objects)
    (assert (is-object object)))
  (create-term +term-term-type-merge+
               (loop for o in objects
                     collect (wrap-in-term o))))

(defcommand append (array object)
  "Append an object to the end of an array."
  (assert (is-array array))
  (assert (is-object object))
  (create-term +term-term-type-append+
               (list (wrap-in-term array)
                     (wrap-in-term object))))

(defcommand contains (object string &rest strings)
  "Determine if an object contains a field."
  (assert (is-object object))
  (push string strings)
  (dolist (string strings)
    (assert (is-string string)))
  (create-term +term-term-type-contains+
               (cl:append (list (wrap-in-term object))
                          (loop for s in strings
                                collect (wrap-in-term s)))))

;; -----------------------------------------------------------------------------
;; math and logic
;; -----------------------------------------------------------------------------
(defcommand + (number/string &rest numbers/strings)
  "Add a set of numbers, or concat a set of strings."
  (push number/string numbers/strings)
  (dolist (number/string numbers/strings)
    (assert (or (is-number number/string)
                (is-string number/string))))
  (create-term +term-term-type-add+
               (loop for ns in numbers/strings
                     collect (wrap-in-term ns))))

(defcommand - (number &rest numbers)
  "Subtract a set of numbers."
  (push number numbers)
  (dolist (number numbers)
    (assert (is-number number)))
  (create-term +term-term-type-sub+
               (loop for n in numbers
                     collect (wrap-in-term n))))

(defcommand * (number &rest numbers)
  "Multiply a set of numbers."
  (push number numbers)
  (dolist (number numbers)
    (assert (is-number number)))
  (create-term +term-term-type-mul+
               (loop for n in numbers
                     collect (wrap-in-term n))))

(defcommand / (number &rest numbers)
  "Divide a set of numbers."
  (push number numbers)
  (dolist (number numbers)
    (assert (is-number number)))
  (create-term +term-term-type-div+
               (loop for n in numbers
                     collect (wrap-in-term n))))

(defcommand % (number mod)
  "Modulus a number by another."
  (assert (is-number number))
  (assert (is-number mod))
  (create-term +term-term-type-mod+
               (list (wrap-in-term number)
                     (wrap-in-term mod))))

(defcommand && (boolean &rest booleans)
  "Logical and a set of booleans."
  (push boolean booleans)
  (dolist (bool booleans)
    (assert (is-boolean bool)))
  (create-term +term-term-type-all+
               (loop for b in booleans
                     collect (wrap-in-term b))))

(defcommand || (boolean &rest booleans)
  "Logical or a set of booleans."
  (push boolean booleans)
  (dolist (bool booleans)
    (assert (is-boolean bool)))
  (create-term +term-term-type-any+
               (loop for b in booleans
                     collect (wrap-in-term b))))

(defcommand == (object &rest objects)
  "Determine equality of a number of objects."
  (push object objects)
  (dolist (object objects)
    (assert (is-datum object)))
  (create-term +term-term-type-eq+
               (loop for o in objects
                     collect (wrap-in-term o))))

(defcommand != (object &rest objects)
  "Determine inequality of a number of objects."
  (push object objects)
  (dolist (object objects)
    (assert (is-datum object)))
  (create-term +term-term-type-ne+
               (loop for o in objects
                     collect (wrap-in-term o))))

(defcommand < (object &rest objects)
  "Determine if objects are less than each other."
  (push object objects)
  (dolist (object objects)
    (assert (is-datum object)))
  (create-term +term-term-type-lt+
               (loop for o in objects
                     collect (wrap-in-term o))))

(defcommand <= (object &rest objects)
  "Determine if objects are less than/equal to each other."
  (push object objects)
  (dolist (object objects)
    (assert (is-datum object)))
  (create-term +term-term-type-le+
               (loop for o in objects
                     collect (wrap-in-term o))))

(defcommand > (object &rest objects)
  "Determine if objects are greater than each other."
  (push object objects)
  (dolist (object objects)
    (assert (is-datum object)))
  (create-term +term-term-type-gt+
               (loop for o in objects
                     collect (wrap-in-term o))))

(defcommand >= (object &rest objects)
  "Determine if objects are greater than/equal to each other."
  (push object objects)
  (dolist (object objects)
    (assert (is-datum object)))
  (create-term +term-term-type-ge+
               (loop for o in objects
                     collect (wrap-in-term o))))

(defcommand ~ (boolean)
  "Logical not a boolean value."
  (assert (is-boolean boolean))
  (create-term +term-term-type-not+ (list (wrap-in-term boolean))))

;; -----------------------------------------------------------------------------
;; control structures
;; -----------------------------------------------------------------------------
(defcommand do (function &rest args)
  "Evaluate the given function in the contex of the given arguments."
  (assert (is-function function))
  (assert-fn-args function (length args))
  (create-term +term-term-type-funcall+
               (cl:append (list (wrap-in-term function))
                          (loop for a in args collect (wrap-in-term a)))))

(defcommand branch (bool true-expr false-expr)
  "Given a form that evaluates to a boolean, run the true-expr if it results in
   true, and false-expr if it results in false."
  (assert (is-boolean bool))
  (create-term +term-term-type-branch+
               (list (wrap-in-term bool)
                     (wrap-in-term true-expr)
                     (wrap-in-term false-expr))))

(defcommand foreach (sequence function)
  "Given a sequence, run the given function on each item in the sequence. The
   function takes only one argument."
  (assert (is-sequence sequence))
  (assert (is-function function))
  (assert-fn-args function 1)
  (create-term +term-term-type-foreach+
               (list (wrap-in-term sequence)
                     (wrap-in-term function))))

(defcommand error (message)
  "Throw a runtime error with the given message."
  (assert (is-string message))
  (create-term +term-term-type-error+ (list (wrap-in-term message))))

(defcommand expr (object)
  "Make sure the passed object is able to be passed as an object in a query."
  (wrap-in-term object))

(defcommand js (javascript-str)
  "Takes a string of javascript and executes it on Rethink's V8 engine. Can also
   evaluate to a function and be used in places where functions are accepted,
   however it is always preferred to us (fn ...) instead."
  (assert (is-string javascript-str))
  (create-term +term-term-type-javascript+ (list (wrap-in-term javascript-str))))

(defcommand coerce-to (object type)
  "Convert the given object to the specified type. To determine the type of an
   object, typeof may be used."
  (assert (is-string type))
  (create-term +term-term-type-coerce-to+
               (list (wrap-in-term object)
                     (wrap-in-term type))))

(defcommand typeof (object)
  "Return the string type of the given object."
  (create-term +term-term-type-typeof+ (list (wrap-in-term object))))

