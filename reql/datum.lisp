(in-package :cl-rethinkdb-reql)

(defun create-datum (value &key null-datum)
  "Create a single datum/term object given a value. tries to guess what datum
   type to use given the value.

   Since nil is a boolean, if you want to create a null datum, you'll have to
   set the :null-datum keyword to t (in which case value is ignored and a null
   datum is created/returned)"
  (let ((type (if null-datum
                  nil
                  (type-of value)))
        (datum (make-instance 'datum))
        (datum-type nil)
        (datum-slot nil))
    (cond ((subtypep type 'string)
           (setf value (pb:string-field value)
                 datum-type +datum-datum-type-r-str+
                 datum-slot 'r-str))
          ((subtypep type 'real)
           (setf value (coerce value 'double-float)
                 datum-type +datum-datum-type-r-num+
                 datum-slot 'r-num))
          ((subtypep type 'boolean)
           (setf datum-type +datum-datum-type-r-bool+
                 datum-slot 'r-bool))
          ((subtypep type 'hash-table)
           (setf value (datum-hash value)
                 datum-type +datum-datum-type-r-object+
                 datum-slot 'r-object))
          ((alistp value)
           (setf value (datum-alist value)
                 datum-type +datum-datum-type-r-object+
                 datum-slot 'r-object))
          ((subtypep type '(or list array))
           (setf value (datum-array value)
                 datum-type +datum-datum-type-r-array+
                 datum-slot 'r-array)))
    (when datum-type
      (setf (type datum) datum-type)
      (when datum-slot
        (setf (slot-value datum datum-slot) value)))
    datum))

(defun datum-hash (hash)
  "Given a hash table, create an array of datum pairs. Recurses to create-datum
   to have datums all the way down."
  (let* ((num-items (hash-table-count hash))
         (final (make-array num-items
                            :element-type 'datum-assoc-pair
                            :fill-pointer num-items :adjustable t)))
    (loop for i from 0
          for key being the hash-keys of hash
          for val being the hash-values of hash do
      (let ((datum-assoc (make-instance 'datum-assoc-pair)))
        (setf (key datum-assoc) (pb:string-field (string key))
              (val datum-assoc) (create-datum val)
              (aref final i) datum-assoc)))
    final))

(defun datum-alist (alist)
  "Given an alist, create an array of datum pairs. Recurses to create-datum to
   have datums all the way down."
  (let* ((num-items (length alist))
         (final (make-array num-items
                            :element-type 'datum-assoc-pair
                            :fill-pointer num-items :adjustable t))
         (idx 0))
    (dolist (item alist)
      (let ((key (car item))
            (val (cdr item))
            (datum-assoc (make-instance 'datum-assoc-pair)))
        (setf (key datum-assoc) (pb:string-field (string key))
              (val datum-assoc) (create-datum val)
              (aref final idx) datum-assoc))
      (incf idx))
    final))

(defun datum-array (list/array)
  "Given a list/array, create a datum array. Recurses to create-datum for each
   of its values to have datums all the way down."
  (let* ((num-items (length list/array))
         (is-array (arrayp list/array))
         (final (make-array num-items
                            :element-type 'datum
                            :fill-pointer num-items :adjustable t)))
    (dotimes (i num-items)
      (let* ((item (if is-array
                       (aref list/array i)
                       (car list/array)))
             (datum (if (subtypep (type-of item) 'datum)
                        item
                        (create-datum item))))
        (setf (aref final i) datum)
        (unless is-array
          (setf list/array (cdr list/array)))))
    final))

(defun datum-term (datum)
  "Wraps a term around a datum."
  (let ((term (make-instance 'term)))
    (setf (type term) +term-term-type-datum+
          (datum term) datum)
    term))

