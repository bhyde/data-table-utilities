(in-package #:cl-user)

(defpackage #:data-table-utilities
  (:use #:common-lisp #:data-table)
  (:export
   #:select-rows
   #:compute-column
   #:data-table-of-samples
   #:remove-duplicate-rows
   #:unique-values-of-column
   #:data-table-of-samples))

(in-package #:data-table-utilities)

(let ((known-row-functions (make-hash-table :test #'equal)))
  (defun get-or-make-row-function (row-id data-table predicate-form)
    "Converts a from that may contain column names in to a function of one argument 
on the list that represents a row."
    (let* ((column-names (or (column-names data-table)
                             (error "Can not compile predicate: no column-names")))
           (key (cons predicate-form column-names)))
      (labels ((recure (form)
                 (typecase form
                   (symbol
                    (let ((p? (position form column-names :test #'string-equal)))
                      (if p? `(nth ,p? ,row-id) form)))
                   (list
                    `(,(car form) ,@(mapcar #'recure (rest form))))
                   (t
                    form))))
        (or (gethash key known-row-functions)
            (setf (gethash key known-row-functions)
                  (compile nil `(lambda (,row-id) ,(recure predicate-form)))))))))

(defun select-rows (data-table predicate-form)
  "Return a new data-table containing only the rows that pass the predicate."
  (make-instance 
   'data-table
   :column-names (column-names data-table)
   :column-types (column-types data-table)
   :rows (loop 
            with predicate-fn = (get-or-make-row-function 'a-row data-table predicate-form)
            for row in (rows data-table)
            when (funcall predicate-fn row)
            collect row)))

(defun compute-column (data-table column-name column-form)
  "Replace the values in a column as computed by the form."
  (loop
     with pos = (or (position column-name (column-names data-table) :test #'string=)
                    (error "Column ~S not found" column-name))
     with type = (nth pos (column-types data-table))
     with column-fn = (get-or-make-row-function 'a-row data-table column-form)
     for row in (rows data-table)
     as new-value = (funcall column-fn row)
     do (assert (typep new-value type))
       (setf (nth pos row) (funcall column-fn row)))
  data-table)

(defun data-table-of-samples (data-table &key (sample-size 10))
  "Return a new table of with the given number of rows sampled form the given table."
  (make-instance 
   'data-table
   :rows (data-table::sample-rows (rows data-table) :sample-size sample-size)
   :column-types (column-types data-table)
   :column-names (column-names data-table)))

(defun remove-duplicate-rows (data-table &key (test #'equal) (key-form nil))
  "Remove dupliate rows using test, which defaults to equal"
  (setf (rows data-table)
        (loop
           with key-fn = (if key-form 
                             (get-or-make-row-function key-form)
                             #'identity)
           with tbl = (make-hash-table :test test)
           for row in (rows data-table)
           as hask-key = (funcall key-fn row)
           unless (gethash row tbl)
           collect (progn (setf (gethash row tbl) t)
                          row)))
  data-table)

(defun unique-values-of-column (data-table column &key (test #'equal))
  (loop 
     with idx = (position column (column-names data-table))
     with tbl = (make-hash-table :test test)
     for row in (rows data-table)
     as value = (nth idx row)
     unless (gethash value tbl) 
     collect (progn
               (setf (gethash value tbl) t)
               value)))

