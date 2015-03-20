(in-package :cl-rethinkdb-reql)

(defparameter *commands* (make-hash-table :test 'eq)
  "Holds name -> lambda mappings for our commands.")

(defun cmd-arg (x)
  "Makes sure all DATA (not command) arrays are wrapped in a make-array cmd."
  (cond ((alistp x)
         (let ((hash (make-hash-table :test #'equal)))
           (loop for (k . v) in x do
             (setf (gethash k hash) (cmd-arg v)))
           hash))
        ((and (listp x)
              (not (null x)))
         (apply 'make-array x))
        ((and (vectorp x)
              (not (stringp x)))
         (apply 'make-array (coerce x 'list)))
        ((hash-table-p x)
         (loop for k being the hash-keys of x
               for v being the hash-values of x do
           (setf (gethash k x) (cmd-arg v)))
         x)
        (t x)))

(defclass reql-cmd ()
  ((name :accessor cmd-name :initarg :name :initform "")
   (op :accessor cmd-op :initarg :op :initform 0)
   (args :accessor cmd-args :initarg :args :initform nil)
   (options :accessor cmd-options :initarg :options :initform (hu:hash)))
  (:documentation
    "Describes a REQL command."))

(defmethod make-load-form ((cmd reql-cmd) &optional env)
  (declare (ignore env))
  (make-load-form-saving-slots cmd))

(defmethod print-object ((cmd reql-cmd) s)
  (print-unreadable-object (cmd s :type t :identity t)
    (format s "~_~a/~a ~s "
            (cmd-name cmd)
            (cmd-op cmd)
            (cmd-args cmd))
    (yason:encode (cmd-options cmd) s)))
            
(defmethod yason:encode ((cmd reql-cmd) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:with-array ()
      (let ((elements (cl:append
                        (list (cmd-op cmd)
                              (cmd-args cmd))
                        (unless (zerop (hash-table-count (cmd-options cmd)))
                          (list (cmd-options cmd))))))
        (apply 'yason:encode-array-elements elements)))))

(defmacro defcommand ((termval name &key (defun t)) all-args &key docstr arrays)
  "Wraps creation of commands."
  (let* ((restpos (or (position '&rest all-args) (length all-args)))
         (keypos (or (position '&key all-args) (length all-args)))
         (args (subseq all-args 0 (cl:min restpos keypos)))
         (restargs (subseq all-args (cl:min (length all-args) (1+ restpos))))
         (optargs (subseq all-args (cl:min (length all-args) (1+ keypos))))
         (optarg-keys (mapcar (lambda (x) (cl-ppcre:regex-replace-all "-" (string-downcase (string x)) "_"))
                              optargs))
         (optargs-processed (mapcar (lambda (x) (list x nil (intern (concatenate 'string (string x) "-PASSED-P"))))
                                    optargs))
         (lambda-list (cl:append args
                                 (unless (zerop (length restargs)) (list '&rest (car restargs)))
                                 (unless (zerop (length optargs)) '(&key))
                                 optargs-processed))
         (process-args (lambda ()
                         (loop for x in args collect
                           (if (find x arrays)
                               `(if (and (listp ,x)
                                         (not (null ,x)))
                                    ,x
                                    (list ,x))
                               `(list ,x)))))
         (hash-sym (gensym "hash"))
         (name-keyword (intern (string name) :keyword)))
    `(let ((fn (lambda ,lambda-list
                 (let ((,hash-sym (hu:hash)))
                   ,@(loop for key in optargs
                           for jskey in optarg-keys
                           for default in optargs-processed collect
                       `(when ,(caddr default)
                          (setf (gethash ,jskey ,hash-sym) ,key)))
                   (make-instance 'reql-cmd
                                  :name ,name-keyword
                                  :op ,termval
                                  :args (or (mapcar 'cmd-arg
                                                    (cl:append ,@(funcall process-args)
                                                               ,(car restargs)))
                                            #())
                                  :options ,hash-sym))))
           (key ,(intern (format nil "~a-~a" (string-upcase (string name)) (length args)) :keyword)))
       (setf (gethash key *commands*) fn)
       ,(when defun
          `(defun ,(if (typep defun 'boolean)
                       name
                       defun)
                  ,lambda-list
             ,docstr
             (apply fn (cl:append (cl:append (list ,@args) ,(car restargs))
                                  ,@(loop for default in optargs-processed
                                          for x = (car default) collect
                                      `(when ,(caddr default)
                                         (list ,(intern (string x) :keyword) ,x))))))))))

(defun call (fn &rest all-args)
  "Call a command by name."
  (let* ((keypos (or (position-if (lambda (x) (keywordp x)) all-args) (length all-args)))
         (args (subseq all-args 0 keypos))
         (optargs (subseq all-args (cl:min (length all-args) keypos)))
         (op-orig fn)
         (op-key (intern (string-upcase (format nil "~a-~a" fn (length args))) :keyword))
         (op-key-def1 (intern (string-upcase (format nil "~a-0" fn)) :keyword))
         (op-key-def2 (intern (string-upcase (format nil "~a-1" fn)) :keyword))
         (fn (gethash op-key *commands*))
         (fn (if fn
                 fn
                 (gethash op-key-def1 *commands*)))
         (fn (if fn
                 fn
                 (gethash op-key-def2 *commands*))))
    (if fn
        (apply fn (cl:append args
                             optargs))
        (cl:error (format nil "command ~s (of ~a arguments) not found" op-orig (length args))))))

(defcommand (2 make-array) (&rest objects))
(defcommand (3 make-obj) (hash))
(defcommand (10 var) (varnumber))
(defcommand (11 javascript) (string &key timeout))
(defcommand (169 uuid) ())
(defcommand (153 http) (url &key data timeout method params header attemps redirects verify page page-limit auth result-format))
(defcommand (12 error) (errstr))
(defcommand (13 implicit-var) ())

(defcommand (14 db) (dbname))
(defcommand (15 table) (db tablename))
(defcommand (15 table :defun nil) (tablename))
(defcommand (16 get) (table id))
(defcommand (78 get-all) (table ids &key index) :arrays (ids))
(defcommand (17 ==) (&rest objects))
(defcommand (18 !=) (&rest objects))
(defcommand (19 <) (&rest objects))
(defcommand (20 <=) (&rest objects))
(defcommand (21 >) (&rest objects))
(defcommand (22 >=) (&rest objects))
(defcommand (23 ~) (bool))
(defcommand (24 +) (&rest objects))
(defcommand (25 -) (&rest objects))
(defcommand (26 *) (&rest objects))
(defcommand (27 /) (&rest objects))
(defcommand (28 %) (number mod))

(defcommand (29 append) (array object))
(defcommand (80 prepend) (array object))
(defcommand (95 difference) (array1 array2))
(defcommand (88 set-insert) (array object))
(defcommand (89 set-intersection) (array1 array2))
(defcommand (90 set-union) (array1 array2))
(defcommand (91 set-difference) (array1 array2))

(defcommand (30 slice) (sequence start end))
(defcommand (70 skip) (sequence number))
(defcommand (71 limit) (sequence number))
(defcommand (87 indexes-of) (sequence object/function))
(defcommand (93 contains) (sequence object/function))

(defcommand (31 attr) (object key))
(defcommand (94 keys) (object))
(defcommand (143 object) (&rest pairs))
(defcommand (32 has-fields) (object &rest pathspec))
(defcommand (96 with-fields) (sequence &rest pathspec))
(defcommand (33 pluck) (sequence &rest pathspec))
(defcommand (34 without) (sequence &rest pathspec))
(defcommand (35 merge) (&rest objects))

(defcommand (36 between) (stream left right &key index right-bound left-bound))
(defcommand (37 reduce) (sequence function))
(defcommand (38 map) (sequence function))

(defcommand (39 filter) (sequence function &key default))
(defcommand (40 concat-map) (sequence function))
(defcommand (41 order-by) (sequence fields &key index) :arrays (fields))
(defcommand (41 order-by :defun nil) (sequence &rest fields))
(defcommand (42 distinct) (sequence))
(defcommand (43 count) (sequence))
(defcommand (86 is-empty) (sequence))
(defcommand (44 union) (&rest sequences))
(defcommand (45 nth) (sequence index))
(defcommand (170 bracket) (sequence/object number/string))

(defcommand (48 inner-join) (sequence1 sequence2 function))
(defcommand (49 outer-join) (sequence1 sequence2 function))
(defcommand (50 eq-join) (sequence1 field sequence2 &key index))
(defcommand (72 zip) (sequence))
(defcommand (173 range) (lower upper))
(defcommand (173 range :defun nil) (upper))
(defcommand (173 range :defun nil) ())

(defcommand (82 insert-at) (array index val))
(defcommand (83 delete-at) (array index))
(defcommand (83 delete-at :defun nil) (array start end))
(defcommand (84 change-at) (array index object))
(defcommand (85 splice-at) (array1 index array2))

(defcommand (51 coerce-to) (val string))
(defcommand (52 type-of) (val))

(defcommand (53 update) (selection object/function &key non-atomic durability return-changes))
(defcommand (54 delete) (selection &key durability return-changes))
(defcommand (55 replace) (selection function &key non-atomic durability return-changes))
(defcommand (56 insert) (table object &key conflict durability return-changes))

(defcommand (57 db-create) (name))
(defcommand (58 db-drop) (name))
(defcommand (59 db-list) ())

(defcommand (60 table-create) (db name &key primary-key shards replicas primary-replica-tag))
(defcommand (60 table-create :defun nil) (name &key primary-key shards replicas primary-replica-tag))
(defcommand (61 table-drop) (db name))
(defcommand (61 table-drop :defun nil) (name))
(defcommand (62 table-list) (db))
(defcommand (62 table-list :defun nil) ())
(defcommand (174 config) (db/table))
(defcommand (175 status) (table))
(defcommand (177 wait) (db/table))
(defcommand (176 reconfigure) (db/table &key shards replicas primary-replica-tag dry-run))
(defcommand (179 rebalance) (db/table))

(defcommand (138 sync) (table))

(defcommand (75 index-create) (table name function &key multi geo))
(defcommand (75 index-create :defun nil) (table name &key multi geo))
(defcommand (76 index-drop) (table name))
(defcommand (77 index-list) (table))
(defcommand (139 index-status) (table &rest names))
(defcommand (140 index-wait) (table &rest names))
(defcommand (156 index-rename) (table from to &key overwrite))

(defcommand (64 do) (function &rest args))
(defcommand (65 branch) (bool true-expr false-expr))
(defcommand (66 ||) (&rest bools))
(defcommand (67 &&) (&rest bools))
(defcommand (68 for-each) (sequence function))

(defcommand (69 func) (args body))
(defcommand (73 asc) (string))
(defcommand (74 desc) (string))

(defcommand (79 info) (object))

(defcommand (97 match) (string regex))

(defcommand (141 upcase) (string))
(defcommand (142 downcase) (string))

(defcommand (81 sample) (sequence number))

(defcommand (92 default) (object default))

(defcommand (98 json) (string))
(defcommand (172 to-json-string) (object))

(defcommand (99 iso8601) (string))
(defcommand (100 to-iso8601) (time))
(defcommand (101 epoch-time) (number))
(defcommand (102 to-epoch-time) (time))
(defcommand (103 now) ())
(defcommand (104 in-timezone) (time string))
(defcommand (105 during) (time start end))
(defcommand (106 date) (time))
(defcommand (126 time-of-day) (time))
(defcommand (127 timezone) (time))

(defcommand (128 year) (time))
(defcommand (129 month) (time))
(defcommand (130 day) (time))
(defcommand (131 day-of-week) (time))
(defcommand (132 day-of-year) (time))
(defcommand (133 hours) (time))
(defcommand (134 minutes) (time))
(defcommand (135 seconds) (time))
(defcommand (136 time) (year month day hour minute second timezone))
(defcommand (136 time :defun nil) (year month day timezone))
(defcommand (107 monday) ())
(defcommand (108 tuesday) ())
(defcommand (109 wednesday) ())
(defcommand (110 thursday) ())
(defcommand (111 friday) ())
(defcommand (112 saturday) ())
(defcommand (113 sunday) ())
(defcommand (114 january) ())
(defcommand (115 february) ())
(defcommand (116 march) ())
(defcommand (117 april) ())
(defcommand (117 may) ())
(defcommand (119 june) ())
(defcommand (120 july) ())
(defcommand (121 august) ())
(defcommand (122 september) ())
(defcommand (123 october) ())
(defcommand (124 november) ())
(defcommand (125 december) ())

(defcommand (137 literal) (object))

(defcommand (144 group) (sequence field/function &key index))
(defcommand (145 sum) (sequence field/function))
(defcommand (146 avg) (sequence field/function))
(defcommand (147 min) (sequence field/function))
(defcommand (148 max) (sequence field/function))

(defcommand (149 split) (string &rest args))

(defcommand (150 ungroup) (grouped))

(defcommand (151 random) (lower-bound upper-bound &key float))
(defcommand (151 random :defun nil) (upper-bound &key float))
(defcommand (151 random :defun nil) (&key float))

(defcommand (152 changes) (table))
(defcommand (154 args) (array))

;(defcommand (155 binary) (string))

(defcommand (157 geojson) (object))
(defcommand (158 to-geojson) (geo))
(defcommand (159 point) (lat long))
(defcommand (160 line) (&rest array/geo))
(defcommand (161 polygon) (&rest array/geo))
(defcommand (162 distance) (geo-from geo-to &key geo-system unit))
(defcommand (163 intersects) (geo1 geo2))
(defcommand (164 includes) (geo1 geo2))
(defcommand (165 circle) (geo radius &key num-vertices geo-system unit fill))
(defcommand (166 get-intersecting) (table geo &key index))
(defcommand (167 fill) (geo))
(defcommand (168 get-nearest) (table geo &key index max-results max-dist geo-system unit))
(defcommand (171 polygon-sub) (geo1 geo2))

