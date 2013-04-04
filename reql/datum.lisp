(in-package :cl-rethinkdb-reql)

(defun create-datum (value &key null-datum)
  "Create a single datum/term object given a value. tries to guess what datum
   type to use given the value.

   Since nil is a boolean, if you want to create a null datum, you'll have to
   set the :null-datum keyword to t (in which case value is ignored and a null
   datum is created/returned)"
  (let ((datum (make-instance 'datum))
        (datum-type nil)
        (datum-accessor nil))
    (unless null-datum
      (cond ((typep value 'string)
             (setf value (pb:string-field value)
                   datum-type +datum-datum-type-r-str+
                   datum-accessor 'r-str))
            ((typep value 'real)
             (setf value (coerce value 'double-float)
                   datum-type +datum-datum-type-r-num+
                   datum-accessor 'r-num))
            ((typep value 'boolean)
             (setf datum-type +datum-datum-type-r-bool+
                   datum-accessor 'r-bool))
            ((typep value 'object-collection)
             (setf value (datum-array value)
                   datum-type +datum-datum-type-r-array+
                   datum-accessor 'r-array))
            ((typep value 'object)
             (setf value (datum-object value)
                   datum-type +datum-datum-type-r-object+
                   datum-accessor 'r-object))))
    (when datum-type
      (setf (type datum) datum-type)
      (when datum-accessor
        (funcall (fdefinition (list 'setf datum-accessor)) value datum)))
    datum))

(defun datum-object (hash/alist)
  "Given a hash or alist object, create an array of datum pairs. Recurses to
   create-datum for all values (turtles all the way down)."
  (let* ((num-items (if (hash-table-p hash/alist)
                        (hash-table-count hash/alist)
                        (length hash/alist)))
         (final (make-array num-items
                            :element-type 'datum-assoc-pair
                            :fill-pointer num-items :adjustable t))
         (idx 0))
    (do-hash/alist ((key val) hash/alist)
      (let ((datum-assoc (make-instance 'datum-assoc-pair)))
        (setf (key datum-assoc) (pb:string-field (string key))
              (val datum-assoc) (create-datum val)
              (aref final idx) datum-assoc))
      (incf idx))
    final))

(defun datum-array (list/array)
  "Given a list/array, create a datum array. Recurses to create-datum for each
   of its values to have datums all the way down."
  (let* ((num-items (length list/array))
         (final (make-array num-items
                            :element-type 'datum
                            :fill-pointer num-items :adjustable t))
         (idx 0))
    (do-list/vector (item list/array)
      (let ((datum (if (subtypep (type-of item) 'datum)
                       item
                       (create-datum item))))
        (setf (aref final idx) datum))
      (incf idx))
    final))

