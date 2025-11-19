(defpackage soil-align/cache
  (:use #:cl)
  (:local-nicknames (#:util   #:soil-align/util)
                    (#:pca    #:soil-align/pca)
                    (#:pre    #:soil-align/preprocessing)
                    (#:sift3d #:soil-align/sift3d))
  (:export #:descriptors-cached))
(in-package :soil-align/cache)

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

(serapeum:-> format-hash ((simple-array (unsigned-byte 8) (32)))
             (values string &optional))
(defun format-hash (hash)
  (with-output-to-string (out)
    (loop for x across hash do
          (format out "~16,2,'0R" x))))

(serapeum:-> write-descriptors
             (pathname
              double-float
              (util:fixed-entries #.util:+descriptor-offset+)
              (util:fixed-entries *)
              (util:fixed-entries #.util:+descriptor-length+)
              (simple-array single-float (#.util:+descriptor-length+)))
             (values &optional))
(defun write-descriptors (pathname threshold coords pca vt means)
  (with-open-file (out pathname
                       :direction         :output
                       :if-does-not-exist :create
                       :if-exists         :supersede
                       :element-type      '(unsigned-byte 8))
    (flet ((encode (x)
             (conspack:encode x :stream out)))
      (encode threshold)
      (encode coords)
      (encode pca)
      (encode vt)
      (encode means)))
  (values))

(serapeum:-> read-peak-threshold (pathname)
             (values (or double-float null) &optional))
(defun read-peak-threshold (pathname)
  (nth-value
   0 (ignore-errors
       (with-open-file (in pathname :element-type '(unsigned-byte 8))
         (conspack:decode-stream in)))))

(serapeum:-> read-descriptors (pathname)
             (values
              (util:fixed-entries #.util:+descriptor-offset+)
              (util:fixed-entries *)
              (util:fixed-entries #.util:+descriptor-length+)
              (simple-array single-float (#.util:+descriptor-length+))
              &optional))
(defun read-descriptors (pathname)
  (with-open-file (in pathname :element-type '(unsigned-byte 8))
    (let ((threshold (conspack:decode-stream in))
          (coords    (conspack:decode-stream in))
          (pca       (conspack:decode-stream in))
          (means     (conspack:decode-stream in))
          (vt        (conspack:decode-stream in)))
      (declare (ignore threshold))
      (values coords pca means vt))))

(serapeum:-> descriptors-cached
             ((util:image (unsigned-byte 8)) pathname
              &optional (double-float 0d0 1d0))
             (values (util:fixed-entries #.util:+descriptor-offset+)
                     (util:fixed-entries *)
                     (util:fixed-entries #.util:+descriptor-length+)
                     (simple-array single-float (#.util:+descriptor-length+))
                     &optional))
(defun descriptors-cached (array cache-pathname &optional (peak-threshold 1d-1))
  "Calculate image descriptors using 3D SIFT and cache them in the
cache.  The next time the descriptors are calculated for this
particular array the results are read from the cache. The cache uses
SHA256 hash of the array as a key. Unlike
@c(SOIL-ALIGN/SIFT3D:DESCRIPTORS) function, this function accepts an
(original) array of octets which is later converted to an array of
single floats using CLAHE algorithm. @c(CACHE-PATHNAME) argument is a
path to the cache.

Return three values: Coordinates of keypoints, descriptors in the PCA
space, a transform from the descriptor space to the PCA space,
descriptor components means."
  (let* ((hash (image-hash array))
         (cache-pathname (uiop:ensure-directory-pathname cache-pathname))
         (entry-pathname (merge-pathnames (pathname (format-hash hash)) cache-pathname))
         (peak-threshold-cached (read-peak-threshold entry-pathname)))
    (if (and peak-threshold-cached (<= peak-threshold-cached peak-threshold))
        ;; Descriptors are in the cahce
        (read-descriptors entry-pathname)
        ;; otherwise compute them
        (multiple-value-bind (coord descr)
            (sift3d:descriptors (pre:clahe array) peak-threshold)
          (when (< (array-dimension descr 0)
                   (array-dimension descr 1))
            (error 'util:db-error :message "Too small number of feature points"))
          (multiple-value-bind (vt means)
              (pca:fit-pca descr 0.95)
            (let ((pca (pca:transform-pca descr vt means)))
              (ensure-directories-exist cache-pathname)
              (write-descriptors entry-pathname peak-threshold coord pca vt means)
              (values coord pca vt means)))))))
