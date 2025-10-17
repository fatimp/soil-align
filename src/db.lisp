(defpackage soil-align/db
  (:use #:cl)
  (:local-nicknames (#:util   #:soil-align/util)
                    (#:sift3d #:soil-align/sift3d))
  (:export #:descriptors-cached))
(in-package :soil-align/db)

#|
(declaim (inline convert-to-simple-array))
(defun convert-to-simple-array (sequence)
  (coerce sequence '(simple-array (unsigned-byte 8) (*))))

(serapeum:-> image-hash ((util:image (unsigned-byte 8)))
             (values (simple-array (unsigned-byte 8) (32)) &optional))
(defun image-hash (array)
  (let ((digest (ironclad:make-digest 'ironclad:sha256)))
    ;; Update with array dimensions
    (ironclad:update-digest
     digest
     (let ((dim-vector (make-array (* 3 4) :element-type '(unsigned-byte 8))))
       (loop for dim in (array-dimensions array)
             for idx from 0 by 4 do
             (setf (nibbles:ub32ref/le dim-vector idx) dim))
       dim-vector))
    ;; Update with array data
    (ironclad:update-digest
     digest (sb-ext:array-storage-vector array))
    (ironclad:produce-digest digest)))

(serapeum:-> descriptors->ub8-vector ((util:fixed-entries 771))
             (values (simple-array (unsigned-byte 8) (*)) &optional))
(defun descriptors->ub8-vector (descriptors)
  (declare (optimize (speed 3)))
  (let* ((length (array-total-size descriptors))
         (result (make-array (* length 4)
                             :element-type '(unsigned-byte 8))))
    (loop for i below length do
          (setf (nibbles:ieee-single-ref/le result (* i 4))
                (row-major-aref descriptors i)))
    result))

(serapeum:-> ub8-vector->descriptors ((simple-array (unsigned-byte 8) (*)))
             (values (util:fixed-entries 771) &optional))
(defun ub8-vector->descriptors (vector)
  (declare (optimize (speed 3)))
  (let* ((length (length vector))
         (desc-length (+ util:+descriptor-length+ util:+descriptor-offset+))
         (result (make-array (list (/ length desc-length 4) desc-length)
                             :element-type 'single-float)))
    (loop for i below (array-total-size result) do
          (setf (row-major-aref result i)
                (nibbles:ieee-single-ref/le vector (* i 4))))
    result))

(defun prepare-database (db)
  (sqlite:execute-non-query
   db #.(concatenate
         'string
         "create table if not exists descriptors (sha256 blob primary key, "
         "mindog real not null, "
         "descr blob not null);")))

(serapeum:-> descriptors-cached
             ((util:image (unsigned-byte 8))
              (serapeum:-> ((util:image (unsigned-byte 8)))
                           (values (util:image single-float) &optional))
              pathname
              &optional (double-float 0d0 1d0))
             (values (util:fixed-entries 771) &optional))
(defun descriptors-cached (array preprocess db-pathname &optional (peak-threshold 1d-1))
  "Calculate image descriptors using 3D SIFT and cache them in a
database. The next time the descriptors are calculated for this
particular array the results are read from the database. The database
uses SHA256 hash of the array as a key into the database. Unlike
@c(SOIL-ALIGN/SIFT3D:DESCRIPTORS) function, this function accepts an
(original) array of octets which is later converted to an array of
single floats using @c(PREPROCESS). @c(DB-PATHNAME) argument is a path to
the database."
  (let ((hash (image-hash array)))
    (ensure-directories-exist db-pathname)
    (sqlite:with-open-database (db (uiop:native-namestring db-pathname))
      (prepare-database db)
      (multiple-value-bind (peak-threshold-cached descriptors)
          (sqlite:execute-one-row-m-v
           db "select mindog, descr from descriptors where sha256 = ?"
           hash)
        (cond
          ((and peak-threshold-cached (<= peak-threshold-cached peak-threshold))
           ;; Descriptors are in the database
           (ub8-vector->descriptors descriptors))
          (t
           (let ((descriptors (sift3d:descriptors (funcall preprocess array) peak-threshold)))
             (sqlite:with-transaction db
               ;; Drop all stale entries
               (sqlite:execute-non-query
                db "delete from descriptors where sha256 = ?" hash)
               (sqlite:execute-non-query
                db "insert into descriptors (sha256, mindog, descr) values (?, ?, ?)"
                hash peak-threshold
                (descriptors->ub8-vector descriptors)))
             descriptors)))))))
|#

(defun descriptors-cached (array preprocess db-pathname &optional (peak-threshold 1d-1))
  (declare (ignore db-pathname))
  (sift3d:descriptors (funcall preprocess array) peak-threshold))
