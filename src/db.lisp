(defpackage soil-align/db
  (:use #:cl)
  (:local-nicknames (#:util   #:soil-align/util)
                    (#:pca    #:soil-align/pca)
                    (#:pre    #:soil-align/preprocessing)
                    (#:sift3d #:soil-align/sift3d))
  (:export #:descriptors-cached))
(in-package :soil-align/db)

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

(serapeum:-> floats->ub8-vector ((simple-array single-float))
             (values (simple-array (unsigned-byte 8) (*)) &optional))
(defun floats->ub8-vector (array)
  (declare (optimize (speed 3)))
  (let* ((length (array-total-size array))
         (result (make-array (* length 4)
                             :element-type '(unsigned-byte 8))))
    (loop for i below length do
          (setf (nibbles:ieee-single-ref/le result (* i 4))
                (row-major-aref array i)))
    result))

(serapeum:-> ub8-vector->floats
             ((simple-array (unsigned-byte 8) (*))
              (or list alexandria:positive-fixnum))
             (values (simple-array single-float) &optional))
(defun ub8-vector->floats (vector shape)
  (declare (optimize (speed 3)))
  (assert (= (length vector)
             (* (if (atom shape) shape (reduce #'* shape)) 4)))
  (let ((result (make-array shape
                            :element-type 'single-float)))
    (loop for i below (array-total-size result) do
          (setf (row-major-aref result i)
                (nibbles:ieee-single-ref/le vector (* i 4))))
    result))

(defun prepare-database (db)
  (sqlite:execute-non-query
   db #.(concatenate
         'string
         "create table if not exists descriptors ("
         "sha256 blob primary key, "
         "mindog real not null, "
         "nsamples integer not null, "
         "features integer not null, "
         "means blob not null, "
         "vt    blob not null, "
         "pca   blob not null, "
         "coord blob not null);")))

(serapeum:-> descriptors-cached
             ((util:image (unsigned-byte 8)) pathname
              &optional (double-float 0d0 1d0))
             (values (util:fixed-entries #.util:+descriptor-offset+)
                     (util:fixed-entries *)
                     (util:fixed-entries #.util:+descriptor-length+)
                     (simple-array single-float (#.util:+descriptor-length+))
                     &optional))
(defun descriptors-cached (array db-pathname &optional (peak-threshold 1d-1))
  "Calculate image descriptors using 3D SIFT and cache them in a
database. The next time the descriptors are calculated for this
particular array the results are read from the database. The database
uses SHA256 hash of the array as a key into the database. Unlike
@c(SOIL-ALIGN/SIFT3D:DESCRIPTORS) function, this function accepts an
(original) array of octets which is later converted to an array of
single floats using CLAHE algorithm. @c(DB-PATHNAME) argument is a
path to the database.

Return three values: Coordinates of keypoints, descriptors in the PCA
space, a transform from the descriptor space to the PCA space,
descriptor components means."
  (let ((hash (image-hash array)))
    (ensure-directories-exist db-pathname)
    (sqlite:with-open-database (db (uiop:native-namestring db-pathname))
      (prepare-database db)
      (multiple-value-bind (peak-threshold-cached nsamples features means coord vt pca)
          (sqlite:execute-one-row-m-v
           db #.(concatenate 'string
                             "select mindog, nsamples, features, means, coord, "
                             "vt, pca from descriptors where sha256 = ?")
           hash)
        (if (and peak-threshold-cached (<= peak-threshold-cached peak-threshold))
            ;; Descriptors are in the database, decompress them from PCA space
            (let ((pca   (ub8-vector->floats pca   (list nsamples features)))
                  (vt    (ub8-vector->floats vt    (list features util:+descriptor-length+)))
                  (means (ub8-vector->floats means (list util:+descriptor-length+)))
                  (coord (ub8-vector->floats coord (list nsamples util:+descriptor-offset+))))
                (values coord pca vt means))
            ;; else
            (multiple-value-bind (coords descr)
                (sift3d:descriptors (pre:clahe array) peak-threshold)
              (if (< (array-dimension descr 0)
                     (array-dimension descr 1))
                  (error 'util:db-error :message "Too small number of feature points")
                  (multiple-value-bind (vt means)
                      (pca:fit-pca descr 0.95)
                    (let ((pca (pca:transform-pca descr vt means)))
                      (sqlite:with-transaction db
                        ;; Drop all stale entries
                        (sqlite:execute-non-query
                         db "delete from descriptors where sha256 = ?" hash)
                        (sqlite:execute-non-query
                         db #.(concatenate 'string
                                           "insert into descriptors "
                                           "(sha256, mindog, nsamples, features, "
                                           "means, vt, pca, coord) "
                                           "values (?, ?, ?, ?, ?, ?, ?, ?)")
                         hash peak-threshold
                         (array-dimension pca 0) (array-dimension pca 1)
                         (floats->ub8-vector means)
                         (floats->ub8-vector vt)
                         (floats->ub8-vector pca)
                         (floats->ub8-vector coords)))
                      (values coords pca vt means))))))))))
