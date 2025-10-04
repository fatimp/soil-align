(defpackage soil-align/db
  (:use #:cl)
  (:local-nicknames (#:util   #:soil-align/util)
                    (#:sift3d #:soil-align/sift3d))
  (:export #:prepare-database
           #:*db-pathname*
           #:descriptors-cached))
(in-package :soil-align/db)

(declaim (type pathname *db-pathname*))
(defparameter *db-pathname*
  #+unix
  #p"~/.local/share/soil-align/descriptors.sqlite"
  #-unix
  (error "I don't know a suitable location where I can store the database.")
  "Path where the cache is stored")

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
   db "create table if not exists summary (sha256 blob primary key, mindog real not null);")
  (sqlite:execute-non-query
   db #.(concatenate
         'string
         "create table if not exists descriptors (sha256 blob not null, "
         "descr blob not null);")))

(defun prepare-database ()
  (sqlite:with-open-database (db (uiop:native-namestring *db-pathname*))
    (%prepare-database db)))

(serapeum:-> descriptors-cached
             ((util:image (unsigned-byte 8))
              (serapeum:-> ((util:image (unsigned-byte 8)))
                           (values (util:image single-float) &optional))
              &optional (double-float 0d0 1d0))
             (values list &optional))
(defun descriptors-cached (array preprocess &optional (peak-threshold 1d-1))
  "Calculate image descriptors using 3D SIFT and cache them in a
database. The next time the descriptors are calculated for this
particular array the results are read from the database. The database
uses SHA256 hash of the array as a key into the database. Unlike
@c(SOIL-ALIGN/SIFT3D:DESCRIPTORS) function, this function accepts an
(original) array of octets which is later converted to an array of
single floats using @c(PREPROCESS)."
  (let ((hash (image-hash array)))
    (ensure-directories-exist *db-pathname*)
    (sqlite:with-open-database (db (uiop:native-namestring *db-pathname*))
      (%prepare-database db)
      (let ((%peak-threshold
             (sqlite:execute-single
              db "select mindog from summary where sha256 = ?"
              hash)))
        (cond
          ((and %peak-threshold (<= %peak-threshold peak-threshold))
           ;; Descriptors are in the database
           (mapcar (lambda (descr) (ub8-vector->descriptor (car descr)))
                   (sqlite:execute-to-list
                    db "select descr from descriptors where sha256 = ?"
                    hash)))
          (t
           (let ((descriptors (sift3d:descriptors (funcall preprocess array) peak-threshold)))
             (sqlite:with-transaction db
               ;; Drop all stale entries
               (sqlite:execute-non-query
                db "delete from summary where sha256 = ?" hash)
               (sqlite:execute-non-query
                db "delete from descriptors where sha256 = ?" hash)
               (sqlite:execute-non-query
                db "insert into summary (sha256, mindog) values (?, ?)"
                hash peak-threshold)
               (loop for descr in descriptors do
                     (sqlite:execute-non-query
                      db "insert into descriptors (sha256, descr) values (?, ?)"
                      hash (descriptor->ub8-vector descr))))
             descriptors)))))))
