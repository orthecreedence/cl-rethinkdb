(in-package :cl-rethinkdb-reql)

;; -----------------------------------------------------------------------------
;; utils
;; -----------------------------------------------------------------------------
(defun is-select (object)
  "Determine if a given object is a selection term, which returns (possibly)
   either a StreamSelect or SingleSelect object."
  (is-term (list +term-term-type-table+
                 +term-term-type-get+
                 +term-term-type-between+
                 +term-term-type-filter+)
           object))

(defun is-sequence (object)
  "Determine if a given object is a sequence term/object."
  (or (typep object 'list)
      (typep object 'vector)
      (is-select object)
      (is-term (list +term-term-type-slice+
                     +term-term-type-skip+
                     +term-term-type-limit+
                     +term-term-type-pluck+
                     +term-term-type-without+
                     +term-term-type-merge+
                     +term-term-type-map+
                     +term-term-type-filter+
                     +term-term-type-concatmap+
                     +term-term-type-orderby+
                     +term-term-type-distinct+
                     +term-term-type-union+
                     +term-term-type-grouped-map-reduce+
                     +term-term-type-inner-join+
                     +term-term-type-outer-join+
                     +term-term-type-eq-join+
                     +term-term-type-zip+)
               object)
      (and (is-term +term-term-type-datum+ object)
           (= (type (datum object)) +datum-datum-type-r-array+))))

(defun is-array (object)
  "Determine if the given object is an array term/type."
  (or (is-term (list +term-term-type-make-array+
                     +term-term-type-append+
                     +term-term-type-db-list+
                     +term-term-type-table-list+)
               object)))

(defun is-boolean (object)
  "Determine if the given object is a boolean term/type."
  (or (typep object 'boolean)
      (is-term (list +term-term-type-eq+
                     +term-term-type-ne+
                     +term-term-type-lt+
                     +term-term-type-le+
                     +term-term-type-gt+
                     +term-term-type-ge+
                     +term-term-type-not+
                     +term-term-type-contains+
                     +term-term-type-any+
                     +term-term-type-all+)
               object)))

(defun is-object (object)
  "Determines if an object is an object type."
  (or (typep object 'alist)
      (typep object 'hash-table)
      (is-term (list +term-term-type-get+
                     +term-term-type-implicit-var+
                     +term-term-type-getattr+
                     +term-term-type-reduce+
                     +term-term-type-nth+
                     +term-term-type-funcall+
                     +term-term-type-pluck+
                     +term-term-type-without+
                     +term-term-type-merge+
                     +term-term-type-update+
                     +term-term-type-delete+
                     +term-term-type-insert+
                     +term-term-type-db-create+
                     +term-term-type-db-drop+
                     +term-term-type-table-create+
                     +term-term-type-table-drop+
                     +term-term-type-foreach+)
               object)))

(defun is-string (object)
  "Determines if an object can be used as a string term."
  (or (stringp object)
      (is-term +term-term-type-add+ object)))

(defun is-number (object)
  "Determines if an object can be used as a number term."
  (or (realp object)
      (is-term (list +term-term-type-add+
                     +term-term-type-sub+
                     +term-term-type-mul+
                     +term-term-type-div+
                     +term-term-type-mod+
                     +term-term-type-count+)
               object)))

(defun is-pkey (object)
  "Determines if an object is a primary key term."
  (or (typep object 'pkey)
      (is-string object)
      (is-number object)))

(defun wrap-in-term (object)
  "Make sure a sequence type is wrapped in a term."
  (cond ((typep object 'term)
         object)
        ((typep object 'javascript-fn)
         (js object))
        (t
         (term-from-datum (create-datum object)))))

(defmacro assert-fn-args (javascript-fn num-args)
  "Assert that a javascript function has the correct number of arguments."
  (let ((fn (gensym "fn")))
    `(let ((,fn ,javascript-fn))
       (when (typep ,fn 'javascript-fn)
         (assert (= (num-args ,fn) ,num-args))))))

(deftype pkey () '(or real string))

;; -----------------------------------------------------------------------------
;; manipulating databases
;; -----------------------------------------------------------------------------
(defun db-create (db-name)
  "Create a DB."
  (assert (is-string db-name))
  (create-term +term-term-type-db-create+ (list (wrap-in-term db-name))))
  
(defun db-drop (db-name)
  "Drop a DB."
  (assert (is-string db-name))
  (create-term +term-term-type-db-drop+ (list (wrap-in-term db-name))))
  
(defun db-list ()
  "List DBs."
  (create-term +term-term-type-db-list+))

;; -----------------------------------------------------------------------------
;; manipulating tables
;; -----------------------------------------------------------------------------
(defun table-create (database table-name &key datacenter primary-key cache-size)
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
    
(defun table-drop (database table-name)
  "Drop a table from a database."
  (assert (is-term +term-term-type-db+ database))
  (assert (is-string table-name))
  (create-term +term-term-type-table-drop+
               (list (wrap-in-term database)
                     (wrap-in-term table-name))))

(defun table-list (database)
  "List tables in a database."
  (assert (is-term +term-term-type-db+ database))
  (create-term +term-term-type-table-list+ (list (wrap-in-term database))))

;; -----------------------------------------------------------------------------
;; writing data
;; -----------------------------------------------------------------------------
(defun insert (table sequence/object &key upsert)
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

(defun update (select object/javascript-fn &key non-atomic)
  "Update an object or set of objects (a select) using the given object or
   javascript function string. Supports using non-atomic writing via
   :non-atomic."
  (assert (is-select select))
  (assert (typep object/javascript-fn '(or object javascript-fn)))
  (assert-fn-args object/javascript-fn 2)
  (assert (is-boolean non-atomic))
  (create-term +term-term-type-update+
               (list (wrap-in-term select)
                     (wrap-in-term object/javascript-fn))
               (when non-atomic
                 '(("non_atomic" . t)))))

(defun replace (select object/javascript-fn &key non-atomic)
  "Replace an entire object or set of objects (a select) using the given object
   javascript function string. Supports using non-atomic writing via
   :non-atomic.
   
   The replacement object needs to have the primary key in it."
  (assert (is-select select))
  (assert (typep object/javascript-fn '(or object string)))
  (assert-fn-args object/javascript-fn 2)
  (assert (is-boolean non-atomic))
  (create-term +term-term-type-replace+
               (list (wrap-in-term select)
                     (wrap-in-term object/javascript-fn))
               (when non-atomic
                 '(("non_atomic" . t)))))

(defun delete (select)
  "Delete an object or set of objects (a select)."
  (assert (is-select select))
  (create-term +term-term-type-delete+ (list (wrap-in-term select))))

;; -----------------------------------------------------------------------------
;; selecting data
;; -----------------------------------------------------------------------------
(defun db (db-name)
  "Create a database object."
  (assert (is-string db-name))
  (create-term +term-term-type-db+ (list (wrap-in-term db-name))))

(defun table (table-name &key db use-outdated)
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

(defun get (table id)
  "Creates a query to grab an object with the given ID (string or int) from the
   given table."
  (assert (is-term +term-term-type-table+ table))
  (assert (is-pkey id))
  (create-term +term-term-type-get+
               (list (wrap-in-term table)
                     (wrap-in-term id))))

(defun between (select &key left right)
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

(defun filter (sequence object/javascript-fn)
  "Filter a sequence by either an object or a javascript function (string)."
  (assert (is-sequence sequence))
  (assert (typep object/javascript-fn '(or object javascript-fn)))
  (create-term +term-term-type-filter+
               (list (wrap-in-term sequence)
                     (wrap-in-term object/javascript-fn))))

;; -----------------------------------------------------------------------------
;; joins
;; -----------------------------------------------------------------------------
(defun inner-join (sequence1 sequence2 javascript-fn)
  "Perform an inner join on two sequences using the given javascript function."
  (assert (is-sequence sequence1))
  (assert (is-sequence sequence2))
  (assert (typep javascript-fn 'javascript-fn))
  (assert-fn-args javascript-fn 2)
  (create-term +term-term-type-inner-join+
               (list (wrap-in-term sequence1)
                     (wrap-in-term sequence2)
                     (js javascript-fn))))
  
(defun outer-join (sequence1 sequence2 javascript-fn)
  "Perform a left outer join on two sequences using the given javascript
   function."
  (assert (is-sequence sequence1))
  (assert (is-sequence sequence2))
  (assert (typep javascript-fn 'javascript-fn))
  (assert-fn-args javascript-fn 2)
  (create-term +term-term-type-outer-join+
               (list (wrap-in-term sequence1)
                     (wrap-in-term sequence2)
                     (js javascript-fn))))
  
(defun eq-join (sequence1 field sequence2)
  "Perform an equality join on two sequences by the given attribute name."
  (assert (is-sequence sequence1))
  (assert (stringp field))
  (assert (is-sequence sequence2))
  (create-term +term-term-type-eq-join+
               (list (wrap-in-term sequence1)
                     (wrap-in-term field)
                     (wrap-in-term sequence2))))

(defun zip (sequence)
  "Merge left/right fields of each member of a join."
  (assert (is-sequence sequence))
  (create-term +term-term-type-zip+ (list (wrap-in-term sequence))))

;; -----------------------------------------------------------------------------
;; transformations
;; -----------------------------------------------------------------------------
(defun map (sequence javascript-fn)
  "Perform a map (as in map/reduce) on a sequence."
  (assert (is-sequence sequence))
  (assert (typep javascript-fn 'javascript-fn))
  (assert-fn-args javascript-fn 1)
  (create-term +term-term-type-map+
               (list (wrap-in-term sequence)
                     (wrap-in-term javascript-fn))))

(defun concat-map (sequence javascript-fn)
  "Construct a sequence of all elements returned by the given mapping function."
  (assert (is-sequence sequence))
  (assert (typep javascript-fn 'javascript-fn))
  (assert-fn-args javascript-fn 1)
  (create-term +term-term-type-concatmap+
               (list (wrap-in-term sequence)
                     (wrap-in-term javascript-fn))))

(defun order-by (sequence field &rest fields)
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

(defun skip (sequence number)
  "Skip a number of items in a sequence."
  (assert (is-sequence sequence))
  (assert (is-number number))
  (create-term +term-term-type-skip+
               (list (wrap-in-term sequence)
                     (wrap-in-term number))))

(defun limit (sequence number)
  "Limit a sequence by a number."
  (assert (is-sequence sequence))
  (assert (is-number number))
  (create-term +term-term-type-limit+
               (list (wrap-in-term sequence)
                     (wrap-in-term number))))

(defun slice (sequence start end)
  "Slice a sequence by a start and end index value."
  (assert (is-sequence sequence))
  (assert (is-number start))
  (assert (is-number end))
  (create-term +term-term-type-slice+
               (list (wrap-in-term sequence)
                     (wrap-in-term start)
                     (wrap-in-term end))))

(defun nth (sequence number)
  "Get the nth element of a sequence."
  (assert (is-sequence sequence))
  (assert (is-number number))
  (create-term +term-term-type-nth+
               (list (wrap-in-term sequence)
                     (wrap-in-term number))))

(defun union (sequence &rest sequences)
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
(defun reduce (sequence javascript-fn)
  "Perform a reduce on sequence using the given javascript function."
  (assert (is-sequence sequence))
  (assert (typep javascript-fn 'javascript-fn))
  (assert-fn-args javascript-fn 2)
  (create-term +term-term-type-reduce+
               (list (wrap-in-term sequence)
                     (wrap-in-term javascript-fn))))

(defun count (sequence)
  "Counts the items in a sequence."
  (assert (is-sequence sequence))
  (create-term +term-term-type-count+
               (list (wrap-in-term sequence))))

(defun distinct (sequence)
  "Get all the distinct elements in a sequence (ie remove-duplicates)."
  (assert (is-sequence sequence))
  (create-term +term-term-type-distinct+
               (list (wrap-in-term sequence))))

(defun grouped-map-reduce (sequence function-group function-map function-reduce)
  "Partition a sequence into groups then perform map/reduce on those groups."
  (assert (is-sequence sequence))
  (assert (typep function-group 'javascript-fn))
  (assert (typep function-map 'javascript-fn))
  (assert (typep function-reduce 'javascript-fn))
  (assert-fn-args function-group 1)
  (assert-fn-args function-map 1)
  (assert-fn-args function-reduce 2)
  (create-term +term-term-type-grouped-map-reduce+
               (list (wrap-in-term function-group)
                     (wrap-in-term function-map)
                     (wrap-in-term function-reduce))))
;; TODO: build me
(defun group-by ())

;; -----------------------------------------------------------------------------
;; reductions
;; -----------------------------------------------------------------------------

;; how to make "avg" function:
; args {
;   type: FUNC
;   args {
;     type: MAKE_ARRAY
;     args {
;       type: DATUM
;       datum {
;         type: R_NUM
;         r_num: 1
;       }
;     }
;   }
;   args {
;     type: MAKE_OBJ
;     optargs {
;       key: "AVG"
;       val {
;         type: VAR
;         args {
;           type: DATUM
;           datum {
;             type: R_NUM
;             r_num: 1
;           }
;         }
;       }
;     }
;   }
; }

(defparameter count
  nil
  "A count reduction object.")

(defun sum (field)
  "Sum a field as a reduction.")

(defun avg (field)
  "Average a field as a reduction.")

;; -----------------------------------------------------------------------------
;; document manipulation
;; -----------------------------------------------------------------------------
(defun attr (field object)
  "Grab an object attribute from an object. Can be nested:

     (attr \"name\" (attr \"user\" (row)))

   Would be r.row(\"user\")(\"name\") in JS."
  (assert (stringp field))
  (assert (is-object object))
  (create-term +term-term-type-getattr+
               (list (wrap-in-term object)
                     (wrap-in-term field))))

(defun row (&optional field)
  "Return a reference to the current row. Optionally takes a string field, in
   which case it pulls thhat field from the row:

     (row \"age\")

   Which is a quicker way of saying:

     (attr \"age\" (row))"
  (assert (typep field '(or string null)))
  (let ((row (create-term +term-term-type-implicit-var+)))
    (if field
        (attr row field)
        row)))

(defun pluck (sequence/object field &rest fields)
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

(defun without (sequence/object field &rest fields)
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

(defun merge (object &rest objects)
  "Merge objects together (merge their fields into one object)."
  (push object objects)
  (dolist (object objects)
    (assert (is-object object)))
  (create-term +term-term-type-merge+
               (loop for o in objects
                     collect (wrap-in-term o))))

(defun append (array object)
  "Append an object to the end of an array."
  (assert (is-array array))
  (assert (is-object object))
  (create-term +term-term-type-append+
               (list (wrap-in-term array)
                     (wrap-in-term object))))

(defun contains (object string &rest strings)
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
(defun + (number/string &rest numbers/strings)
  "Add a set of numbers, or concat a set of strings."
  (push number/string numbers/strings)
  (dolist (number/string numbers/strings)
    (assert (or (is-number number/string)
                (is-string number/string))))
  (create-term +term-term-type-add+
               (loop for ns in numbers/strings
                     collect (wrap-in-term ns))))

(defun - (number &rest numbers)
  "Subtract a set of numbers."
  (push number numbers)
  (dolist (number numbers)
    (assert (is-number number)))
  (create-term +term-term-type-sub+
               (loop for n in numbers
                     collect (wrap-in-term n))))

(defun * (number &rest numbers)
  "Multiply a set of numbers."
  (push number numbers)
  (dolist (number numbers)
    (assert (is-number number)))
  (create-term +term-term-type-mul+
               (loop for n in numbers
                     collect (wrap-in-term n))))

(defun / (number &rest numbers)
  "Divide a set of numbers."
  (push number numbers)
  (dolist (number numbers)
    (assert (is-number number)))
  (create-term +term-term-type-div+
               (loop for n in numbers
                     collect (wrap-in-term n))))

(defun % (number mod)
  "Modulus a number by another."
  (assert (is-number number))
  (assert (is-number mod))
  (create-term +term-term-type-mod+
               (list (wrap-in-term number)
                     (wrap-in-term mod))))

(defun && (boolean &rest booleans)
  "Logical and a set of booleans."
  (push boolean booleans)
  (dolist (bool booleans)
    (assert (is-boolean bool)))
  (create-term +term-term-type-all+
               (loop for b in booleans
                     collect (wrap-in-term b))))

(defun || (boolean &rest booleans)
  "Logical or a set of booleans."
  (push boolean booleans)
  (dolist (bool booleans)
    (assert (is-boolean bool)))
  (create-term +term-term-type-any+
               (loop for b in booleans
                     collect (wrap-in-term b))))

(defun == (object &rest objects)
  "Determine equality of a number of objects."
  (push object objects)
  (dolist (object objects)
    (assert (is-object object)))
  (create-term +term-term-type-eq+
               (loop for o in objects
                     collect (wrap-in-term o))))

(defun != (object &rest objects)
  "Determine inequality of a number of objects."
  (push object objects)
  (dolist (object objects)
    (assert (is-object object)))
  (create-term +term-term-type-ne+
               (loop for o in objects
                     collect (wrap-in-term o))))

(defun < (object &rest objects)
  "Determine if objects are less than each other."
  (push object objects)
  (dolist (object objects)
    (assert (is-object object)))
  (create-term +term-term-type-lt+
               (loop for o in objects
                     collect (wrap-in-term o))))

(defun <= (object &rest objects)
  "Determine if objects are less than/equal to each other."
  (push object objects)
  (dolist (object objects)
    (assert (is-object object)))
  (create-term +term-term-type-le+
               (loop for o in objects
                     collect (wrap-in-term o))))

(defun > (object &rest objects)
  "Determine if objects are greater than each other."
  (push object objects)
  (dolist (object objects)
    (assert (is-object object)))
  (create-term +term-term-type-gt+
               (loop for o in objects
                     collect (wrap-in-term o))))

(defun >= (object &rest objects)
  "Determine if objects are greater than/equal to each other."
  (push object objects)
  (dolist (object objects)
    (assert (is-object object)))
  (create-term +term-term-type-ge+
               (loop for o in objects
                     collect (wrap-in-term o))))

(defun ~ (boolean)
  "Logical not a boolean value."
  (assert (is-boolean boolean))
  (create-term +term-term-type-not+ (list (wrap-in-term boolean))))

;; -----------------------------------------------------------------------------
;; control structures
;; -----------------------------------------------------------------------------
(defun do ())
(defun branch ())
(defun foreach ())
(defun error ())
(defun expr (object &key boolean))
(defun js ())
(defun coerce-to ())
(defun typeof ())

