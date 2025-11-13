(defpackage soil-align/pca
  (:use #:cl)
  (:local-nicknames (#:util #:soil-align/util))
  (:export #:fit-pca
           #:transform-pca
           #:invert-pca))
(in-package :soil-align/pca)

(declaim (inline make-matrix))
(defun make-matrix (storage shape &key (layout :column-major))
  (magicl:make-tensor
   'magicl:matrix/single-float shape
   :layout layout
   :storage (sb-ext:array-storage-vector storage)))

;; Here the array of descriptors is transposed
(serapeum:-> mean-values
             ((simple-array single-float (#.util:+descriptor-length+ *)))
             (values (simple-array single-float (#.util:+descriptor-length+)) &optional))
(defun mean-values (descriptors)
  (declare (optimize (speed 3)))
  (let ((result (make-array util:+descriptor-length+
                            :element-type 'single-float))
        (samples (array-dimension descriptors 1))
        (storage (sb-ext:array-storage-vector descriptors)))
    (loop for i below (length result)
          for start = (array-row-major-index descriptors i 0) do
          (setf (aref result i)
                (/ (vector-sum:sum storage
                                   :start start
                                   :end   (+ start samples))
                   samples)))
    result))

(serapeum:-> fit-pca
             ((util:fixed-entries #.util:+descriptor-length+) (single-float 0.0 1.0))
             (values (util:fixed-entries #.util:+descriptor-length+)
                     (simple-array single-float (#.util:+descriptor-length+)) &optional))
(defun fit-pca (descriptors explained-variance)
  "Return a matrix which projects vectors from the descriptor space to
a PCA space of lesser dimensionality. The parameter
@c(EXPLAINED-VARIANCE) controls dimensionality of that space."
  (declare (optimize (speed 3)))
  ;; # samples >= # features
  (assert (>= (array-dimension descriptors 0)
              (array-dimension descriptors 1)))
  (let* ((transposed (util:transpose-2d descriptors))
         (means (mean-values transposed)))
    (util:loop-array (transposed (i j))
      (let ((mean (aref means i)))
        (setf (aref transposed i j)
              (- (aref transposed i j) mean))))
    (multiple-value-bind (u s vt)
        (magicl:svd (make-matrix transposed (array-dimensions descriptors))
                    :reduced t)
      (declare (ignore u))
      (let* ((s (make-array util:+descriptor-length+
                            :element-type 'single-float
                            :initial-contents (magicl:diag s)))
             (samples (array-dimension descriptors 0))
             ;; explained variance
             (ev (map '(vector single-float)
                      (lambda (x)
                        (/ (expt x 2) (1- samples)))
                      s))
             (total-ev (vector-sum:sum ev))
             (ev-ratio (map '(vector single-float)
                            (lambda (x) (/ x total-ev))
                            ev))
             (cum-ratio (vector-sum:scan ev-ratio))
             (n-components (1+ (or (position-if
                                    (lambda (x) (< x explained-variance))
                                    cum-ratio :from-end t)
                                   0)))
             (storage (magicl::storage vt))
             (result (make-array (list n-components util:+descriptor-length+)
                     :element-type 'single-float)))
        ;; Do not use MAGICL:SLICE. It's incredibly slow.
        (declare (type (simple-array single-float (*)) storage))
        (assert (eq (magicl:layout vt) :row-major))
        (util:loop-array (result (i j))
          (setf (aref result i j)
                (aref storage (+ (* i util:+descriptor-length+) j))))
        (values result means)))))

(serapeum:-> transform-pca
             ((util:fixed-entries #.util:+descriptor-length+)
              (util:fixed-entries #.util:+descriptor-length+)
              (simple-array single-float (#.util:+descriptor-length+)))
             (values (simple-array single-float (* *)) &optional))
(defun transform-pca (descriptors vt means)
  "Apply a transform to the PCA space to descriptors. @c(VT) and
@c(MEANS) are obtained from @c(FIT-PCA)."
  (declare (optimize (speed 3)))
  (let ((transposed (util:transpose-2d descriptors)))
    (util:loop-array (transposed (i j))
      (let ((mean (aref means i)))
        (setf (aref transposed i j)
              (- (aref transposed i j) mean))))
    (let* ((matrix (make-matrix transposed (array-dimensions descriptors)))
           (vt (make-matrix vt (array-dimensions vt) :layout :row-major))
           ;; Compute a transposed result to ease column-major to row-major
           ;; conversion
           (transformed (magicl:mult vt matrix :transb :t))
           ;; Convert back to an ordinary lisp array
           (result (make-array (reverse (magicl:shape transformed))
                               :element-type 'single-float))
           (storage (magicl::storage transformed)))
      (declare (type (simple-array single-float) storage))
      (assert (eq (magicl:layout transformed) :column-major))
      (loop for i below (array-total-size result) do
            (setf (row-major-aref result i)
                  (aref storage i)))
      result)))

(serapeum:-> invert-pca
             ((simple-array single-float (* *))
              (util:fixed-entries #.util:+descriptor-length+)
              (simple-array single-float (#.util:+descriptor-length+)))
             (values (util:fixed-entries #.util:+descriptor-length+) &optional))
(defun invert-pca (pca vt means)
  "This is a (lossy) inversion of @c(TRANSFORM-PCA), i.e. it convects
vectors in the PCA space back into the descriptor space."
  (declare (optimize (speed 3)))
  ;; Compute a transposed result to ease column-major to row-major
  ;; conversion
  (let* ((matrix   (make-matrix pca (reverse (array-dimensions pca))))
         (vt       (make-matrix vt (array-dimensions vt) :layout :row-major))
         (inverted (magicl:mult vt matrix :transa :t))
         (storage  (magicl::storage inverted))
         (result   (make-array (list (array-dimension pca 0) util:+descriptor-length+)
                               :element-type 'single-float)))
    (declare (type (simple-array single-float (*)) storage))
    (assert (eq (magicl:layout inverted) :column-major))
    (loop for i below (array-total-size result) do
          (setf (row-major-aref result i) (aref storage i)))
    (util:loop-array (result (i j))
      (incf (aref result i j) (aref means j)))
    result))
