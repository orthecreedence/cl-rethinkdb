(in-package :cl-rethinkdb-reql)

;; -----------------------------------------------------------------------------
;; utils
;; -----------------------------------------------------------------------------
(defmacro assert-select (term)
  "Assert that a given term is a selection term, which returns (possibly) either
   a StreamSelect or SingleSelect object."
  `(assert (is-term (list +term-term-type-get+
                          +term-term-type-between+
                          +term-term-type-filter+)
                    ,term)))

(defmacro assert-sequence (term)
  `(assert (is-term (list +term-term-type-slice+
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
                    ,term)))

(defmacro assert-fn-args (javascript-fn num-args)
  "Assert that a javascript function has the correct number of arguments."
  (let ((fn (gensym "fn")))
    `(let ((,fn ,javascript-fn))
       (when (typep ,fn 'javascript-fn)
         (assert (= (num-args ,fn) ,num-args))))))

(deftype pkey () '(or real string))

(defun is-string-term (object)
  "Determines if an object can be used as a string term."
  (or (stringp object)
      (is-term +term-term-type-add+ object)))

(defun is-number-term (object)
  "Determines if an object can be used as a number term."
  (or (realp object)
      (is-term (list +term-term-type-add+
                     +term-term-type-sub+
                     +term-term-type-mul+
                     +term-term-type-div+
                     +term-term-type-mod+
                     +term-term-type-count+)
               object)))

(defun is-pkey-term (object)
  "Determines if an object is a primary key term."
  (or (is-string-term object)
      (is-number-term object)))

;; -----------------------------------------------------------------------------
;; manipulating databases
;; -----------------------------------------------------------------------------
(defun db-create (db-name)
  "Create a DB."
  (assert (is-string-term db-name))
  (create-term +term-term-type-db-create+ (list (create-datum db-name))))
  
(defun db-drop (db-name)
  "Drop a DB."
  (assert (is-string-term db-name))
  (create-term +term-term-type-db-drop+ (list (create-datum db-name))))
  
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
  (assert (is-string-term table-name))
  (assert (or (null datacenter)
              (stringp datacenter)))
  (assert (or (null primary-key)
              (stringp primary-key)))
  (assert (or (null cache-size)
              (realp cache-size)))
  (let ((options nil))
    (when datacenter (push (cons "datacanter" datacenter) options))
    (when primary-key (push (cons "primary_key" primary-key) options))
    (when cache-size (push (cons "cache_size" cache-size) options))
    (create-term +term-term-type-table-create+
                 (list database (create-datum table-name))
                 options)))
    
(defun table-drop (database table-name)
  "Drop a table from a database."
  (assert (is-term +term-term-type-db+ database))
  (assert (is-string-term table-name))
  (create-term +term-term-type-table-drop+
               (list database (create-datum table-name))))

(defun table-list (database)
  "List tables in a database."
  (assert (is-term +term-term-type-db+ database))
  (create-term +term-term-type-table-list+ (list database)))

;; -----------------------------------------------------------------------------
;; writing data
;; -----------------------------------------------------------------------------
(defun insert (table value &key upsert)
  "Create an insert query, given a table object and a set of values.

   The value can be a hash table (or alist), or an array of hashes/alists (in
   the case of a multi-insert)."
  (assert (is-term +term-term-type-table+ table))
  (assert (typep value '(or object-collection object)))
  (assert (typep upsert 'boolean))
  (create-term +term-term-type-insert+
               (list table (term-from-datum (create-datum value)))
               (when upsert '(("upsert" . t)))))

(defun update (select object/javascript-fn &key non-atomic)
  "Update an object or set of objects (a select) using the given object or
   javascript function string. Supports using non-atomic writing via
   :non-atomic."
  (assert-select select)
  (assert (typep object/javascript-fn '(or object javascript-fn)))
  (assert-fn-args object/javascript-fn 2)
  (assert (typep non-atomic 'boolean))
  (create-term +term-term-type-update+
               (list select
                     (if (stringp object/javascript-fn)
                         (js object/javascript-fn)
                         (term-from-datum (create-datum object/javascript-fn))))
               (when non-atomic
                 '(("non_atomic" . t)))))

(defun replace (select object/javascript-fn &key non-atomic)
  "Replace an entire object or set of objects (a select) using the given object
   javascript function string. Supports using non-atomic writing via
   :non-atomic.
   
   The replacement object needs to have the primary key in it."
  (assert-select select)
  (assert (typep object/javascript-fn '(or object string)))
  (assert-fn-args object/javascript-fn 2)
  (assert (typep non-atomic 'boolean))
  (create-term +term-term-type-replace+
               (list select
                     (if (stringp object/javascript-fn)
                         (js object/javascript-fn)
                         (term-from-datum (create-datum object/javascript-fn))))
               (when non-atomic
                 '(("non_atomic" . t)))))

(defun delete (select)
  "Delete an object or set of objects (a select)."
  (assert-select select)
  (create-term +term-term-type-delete+ (list select)))

;; -----------------------------------------------------------------------------
;; selecting data
;; -----------------------------------------------------------------------------
(defun db (db-name)
  "Create a database object."
  (assert (is-string-term db-name))
  (create-term +term-term-type-db+ (list (create-datum db-name))))

(defun table (table-name &key db use-outdated)
  "Create a table object. Can optionally specify which database the table
   belongs to via :db.
   
   :use-outdated allows you to specify that out-of-date data is ok when querying
   this table."
  (assert (is-string-term table-name))
  (assert (or (null db)
              (is-term +term-term-type-db+ db)))
  (let ((args (list (create-datum table-name))))
    (when db (push db args))
    (create-term +term-term-type-table+
                 args
                 (when use-outdated '(("use_outdated" . t))))))

(defun get (table id)
  "Creates a query to grab an object with the given ID (string or int) from the
   given table."
  (assert (is-term +term-term-type-table+ table))
  (assert (or (typep id 'pkey)
              (is-pkey-term id)))
  (create-term +term-term-type-get+ (list table (create-datum id))))

(defun between (select &key left right)
  "Grabs objects from a selection where the primary keys are between two values."
  (assert-select select)
  (assert (or (typep left '(or null pkey))
              (is-pkey-term left)))
  (assert (or (typep right '(or null pkey))
              (is-pkey-term right)))
  (let ((options nil))
    (when left (push (cons "left_bound" left) options))
    (when right (push (cons "right_bound" right) options))
    (create-term +term-term-type-between+
                 (list select)
                 options)))

(defun filter (sequence object/javascript-fn)
  "Filter a sequence by either an object or a javascript function (string)."
  (assert-sequence sequence)
  (assert (typep object/javascript-fn '(or object javascript-fn)))
  (create-term +term-term-type-filter+
               (list sequence 
                     (if (stringp object/javascript-fn)
                         (js object/javascript-fn)
                         (term-from-datum (create-datum object/javascript-fn))))))

;; -----------------------------------------------------------------------------
;; joins
;; -----------------------------------------------------------------------------
(defun inner-join (sequence1 sequence2 javascript-fn)
  "Perform an inner join on two sequences using the given javascript function."
  (assert-sequence sequence1)
  (assert-sequence sequence2)
  (assert (typep javascript-fn 'javascript-fn))
  (assert-fn-args javascript-fn 2)
  (create-term +term-term-type-inner-join+ (list sequence1 sequence2 (js javascript-fn))))
  
(defun outer-join (sequence1 sequence2 javascript-fn)
  "Perform a left outer join on two sequences using the given javascript
   function."
  (assert-sequence sequence1)
  (assert-sequence sequence2)
  (assert (typep javascript-fn 'javascript-fn))
  (assert-fn-args javascript-fn 2)
  (create-term +term-term-type-outer-join+ (list sequence1 sequence2 (js javascript-fn))))
  
(defun eq-join (sequence1 field sequence2)
  "Perform an equality join on two sequences by the given attribute name."
  (assert-sequence sequence1)
  (assert (stringp field))
  (assert-sequence sequence2)
  (create-term +term-term-type-eq-join+
               (list sequence1 (term-from-datum (create-term field)) sequence2)))

(defun zip (sequence)
  "Merge left/right fields of each member of a join."
  (assert-sequence sequence)
  (create-term +term-term-type-zip+ (list sequence)))

;; -----------------------------------------------------------------------------
;; transformations
;; -----------------------------------------------------------------------------
(defun map (sequence javascript-fn)
  "Perform a map (as in map/reduce) on a sequence."
  (assert-sequence sequence)
  (assert (typep javascript-fn 'javascript-fn))
  (assert-fn-args javascript-fn 1)
  (create-term +term-term-type-map+ (list sequence (js javascript-fn))))

(defun concat-map (sequence javascript-fn)
  "Construct a sequence of all elements returned by the given mapping function."
  (assert-sequence sequence)
  (assert (typep javascript-fn 'javascript-fn))
  (assert-fn-args javascript-fn 1)
  (create-term +term-term-type-concatmap+ (list sequence (js javascript-fn))))

(defun order-by (sequence field)
  "Order a sequence by a field."
  
(defun skip ())
(defun limit ())
(defun slice ())
(defun nth ())
(defun union ())


;; -----------------------------------------------------------------------------
;; aggregation
;; -----------------------------------------------------------------------------
(defun reduce ())
(defun count ())
(defun distinct ())
(defun grouped-map-reduce ())
(defun group-by ())

;; -----------------------------------------------------------------------------
;; reductions
;; -----------------------------------------------------------------------------

;; TODO: how the hell do i do r.count as a variable, not a function? ie:
;;   r.table('marvel').groupBy('strength', r.count).run(c)

(defun sum ())
(defun avg ())

;; -----------------------------------------------------------------------------
;; document manipulation
;; -----------------------------------------------------------------------------

(defun row ())
(defun pluck ())
(defun without ())
(defun merge ())
(defun append ())
(defun attr ())
(defun contains ())

;; -----------------------------------------------------------------------------
;; math and logic
;; -----------------------------------------------------------------------------
(defun + ())
(defun - ())
(defun * ())
(defun / ())
(defun % ())
(defun && ())
(defun || ())
(defun == ())
(defun != ())
(defun > ())
(defun >= ())
(defun < ())
(defun <= ())
(defun ~ ())

;; -----------------------------------------------------------------------------
;; control structures
;; -----------------------------------------------------------------------------
(defun do ())
(defun branch ())
(defun foreach ())
(defun error ())
(defun expr (object &key boolean))
(defun js ())
(defun coerce ())
(defun type-of ())

