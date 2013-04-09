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

(defun datum-to-lisp (datum &key (array-type :array) (object-type :hash))
  "Given a datum object, recursively convert it to a lisp object or set of lisp
   objects."
  (assert (find object-type '(:hash :alist)))
  (assert (find array-type '(:array :list)))
  (let ((type (when (typep datum 'datum)
                (type datum))))
    (cond ((typep datum 'vector)
           (let* ((size (length datum))
                  (arr (make-array size)))
             (dotimes (i size)
               (setf (aref arr i) (datum-to-lisp (aref datum i) :array-type array-type :object-type object-type)))
             arr))
          ((typep datum 'list)
           (loop for d in datum collect (datum-to-lisp d :array-type array-type :object-type object-type)))
          ((eq type +datum-datum-type-r-str+)
           (pb:string-value (r-str datum)))
          ((eq type +datum-datum-type-r-num+)
           (r-num datum))
          ((eq type +datum-datum-type-r-bool+)
           (r-bool datum))
          ((eq type +datum-datum-type-r-array+)
           (if (eq array-type :array)
               (let* ((items (r-array datum))
                      (size (length items))
                      (arr (make-array size)))
                 (dotimes (i size)
                   (setf (aref arr i) (datum-to-lisp (aref items i) :array-type array-type :object-type object-type)))
                 arr)
               (loop for i across (r-array datum)
                     collect (datum-to-lisp i :array-type array-type :object-type object-type))))
          ((eq type +datum-datum-type-r-object+)
           (if (eq object-type :hash)
               (let* ((hash (make-hash-table :test #'equal)))
                 (loop for pair across (r-object datum) do
                   (let ((key (pb:string-value (key pair)))
                         (val (datum-to-lisp (val pair) :array-type array-type :object-type object-type)))
                     (setf (gethash key hash) val)))
                 hash)
               (let ((alist nil))
                 (loop for pair across (r-object datum) do
                   (let ((key (pb:string-value (key pair)))
                         (val (datum-to-lisp (val pair) :array-type array-type :object-type object-type)))
                     (push (cons key val) alist)))
                 (reverse alist))))
          (t datum))))

