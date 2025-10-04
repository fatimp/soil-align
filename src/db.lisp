(defpackage soil-align/db
  (:use #:cl)
  (:local-nicknames (#:util   #:soil-align/util)
                    (#:sift3d #:soil-align/sift3d))
  (:export #:prepare-database
           #:descriptors-cached))
(in-package :soil-align/db)

(alexandria:define-constant +db-name+
    "descriptors.sqlite"
  :test          #'string=
  :documentation "Name of the database (will be created in the current directory).")

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

(deftype storable-descriptor () '(simple-array (unsigned-byte 8) (#.(* 771 4))))

(serapeum:-> descriptor->ub8-vector (util:descriptor)
             (values storable-descriptor &optional))
(defun descriptor->ub8-vector (descriptor)
  (declare (optimize (speed 3)))
  (let* ((length (length descriptor))
         (result (make-array (* length 4)
                             :element-type '(unsigned-byte 8))))
    (loop for i below length do
          (setf (nibbles:ieee-single-ref/le result (* i 4))
                (aref descriptor i)))
    result))

(serapeum:-> ub8-vector->descriptor (storable-descriptor)
             (values util:descriptor &optional))
(defun ub8-vector->descriptor (storable)
  (declare (optimize (speed 3)))
  (let ((result (make-array (+ util:+descriptor-length+ util:+descriptor-offset+)
                            :element-type 'single-float)))
    (loop for i below (length result) do
          (setf (aref result i)
                (nibbles:ieee-single-ref/le storable (* i 4))))
    result))

(defun %prepare-database (db)
  (sqlite:execute-non-query
   db (concatenate
       'string
       "create table if not exists summary (name string not null unique, "
       "sha256 blob not null, "
       "mindog real not null);"))
  (sqlite:execute-non-query
   db (concatenate
       'string
       "create table if not exists descriptors (name string not null, "
       "descr blob not null);")))

(defun prepare-database ()
  (sqlite:with-open-database (db +db-name+)
    (%prepare-database db)))

(serapeum:-> descriptors-cached
             ((or string pathname)
              (util:image (unsigned-byte 8))
              (serapeum:-> ((util:image (unsigned-byte 8)))
                           (values (util:image single-float) &optional))
              &optional (double-float 0d0 1d0))
             (values list &optional))
(defun descriptors-cached (name array preprocess &optional (peak-threshold 1d-1))
  "Calculate image descriptors using 3D SIFT and cache them in a
database. The next time the descriptors are calculated for this
particular combination of @c(NAME) and @c(ARRAY) the results are read
from the database. The database stores SHA256 hash for images to make
internal consistency checks. @c(NAME) is ment to be a file name which
is stored relatively to the current working directory where the
database is resided. Unlike @c(SOIL-ALIGN/SIFT3D:DESCRIPTORS)
function, this function accepts an (original) array of octets which is
later converted to an array of single floats using @c(PREPROCESS)."
  (let ((hash (image-hash array))
        (name (enough-namestring (truename name) (truename "."))))
    (sqlite:with-open-database (db +db-name+)
      (%prepare-database db)
      (multiple-value-bind (%hash %peak-threshold)
          (sqlite:execute-one-row-m-v
           db "select sha256, mindog from summary where name = ?"
           name)
        (cond
          ((and %hash (equalp %hash hash) (<= %peak-threshold peak-threshold))
           ;; Descriptors are in the database
           (mapcar (lambda (descr) (ub8-vector->descriptor (car descr)))
                   (sqlite:execute-to-list
                    db "select descr from descriptors where name = ?"
                    name)))
          (t
           ;; Drop all stale entries
           (sqlite:with-transaction db
             (sqlite:execute-non-query
              db "delete from summary where name = ?" name)
             (sqlite:execute-non-query
              db "delete from descriptors where name = ?" name))
           (let ((descriptors (sift3d:descriptors (funcall preprocess array) peak-threshold)))
             (sqlite:with-transaction db
               (sqlite:execute-non-query
                db "insert into summary (name, sha256, mindog) values (?, ?, ?)"
                name hash peak-threshold)
               (loop for descr in descriptors do
                     (sqlite:execute-non-query
                      db "insert into descriptors (name, descr) values (?, ?)"
                      name (descriptor->ub8-vector descr))))
             descriptors)))))))
