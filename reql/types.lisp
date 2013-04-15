(in-package :cl-rethinkdb-reql)

(defun alistp (alist)
  "Determine if the given object is an alist."
  (flet ((true-cdr-p (element)
           (and (consp element)
                (cdr element))))
    (and (listp alist)
         (every #'true-cdr-p alist))))

(defun objectp (object)
  "Determine if an object is a hash table or alist."
  (typep object 'object))

(defun object-collection-p (collection)
  "Test if an object is a collection (list or vector) or alists/hashes."
  (and (subtypep (type-of collection) '(or list vector))
       (every #'objectp collection)))

(deftype alist () '(satisfies alistp))
(deftype object () '(or hash-table alist))
(deftype object-collection () '(satisfies object-collection-p))

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
                     +term-term-type-zip+
                     +term-term-type-funcall+
                     +term-term-type-branch+)
               object)
      (and (is-term +term-term-type-datum+ object)
           (= (type (datum object)) +datum-datum-type-r-array+))))

(defun is-array (object)
  "Determine if the given object is an array term/type."
  (or (is-term (list +term-term-type-make-array+
                     +term-term-type-append+
                     +term-term-type-db-list+
                     +term-term-type-table-list+
                     +term-term-type-var+
                     +term-term-type-funcall+
                     +term-term-type-branch+)
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
                     +term-term-type-all+
                     +term-term-type-var+
                     +term-term-type-funcall+
                     +term-term-type-branch+)
               object)))

(defun is-object (object)
  "Determines if an object is an object type."
  (or (typep object 'alist)
      (typep object 'hash-table)
      (is-function object)
      (is-term (list +term-term-type-get+
                     +term-term-type-implicit-var+
                     +term-term-type-var+
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
                     +term-term-type-foreach+
                     +term-term-type-funcall+
                     +term-term-type-branch+)
               object)))

(defun is-string (object)
  "Determines if an object can be used as a string term."
  (or (stringp object)
      (is-function object)
      (is-term (list +term-term-type-add+
                     +term-term-type-var+
                     +term-term-type-funcall+
                     +term-term-type-branch+)
               object)))

(defun is-number (object)
  "Determines if an object can be used as a number term."
  (or (realp object)
      (is-function object)
      (is-term (list +term-term-type-add+
                     +term-term-type-sub+
                     +term-term-type-mul+
                     +term-term-type-div+
                     +term-term-type-mod+
                     +term-term-type-count+
                     +term-term-type-var+
                     +term-term-type-funcall+
                     +term-term-type-branch+)
               object)))

(defun is-datum (object)
  "Test if an object is a datum type."
  (or (is-string object)
      (is-number object)
      (is-object object)
      (is-boolean object)))

(deftype pkey () '(or real string))

(defun is-pkey (object)
  "Determines if an object is a primary key term."
  (or (typep object 'pkey)
      (is-string object)
      (is-number object)))

(defun is-function (object)
  "Test if an object is a term function or reql-function object."
  (or (is-term (list +term-term-type-func+
                     +term-term-type-make-obj+
                     +term-term-type-javascript+
                     +term-term-type-funcall+
                     +term-term-type-var+
                     +term-term-type-branch+)
               object)))


