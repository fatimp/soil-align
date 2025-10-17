(defpackage soil-align/pca
  (:use #:cl)
  (:local-nicknames (#:util #:soil-align/util))
  (:export #:fit-pca
           #:transform-pca
           #:invert-pca))
(in-package :soil-align/pca)

;; Here the array of descriptors is transposed
(serapeum:-> mean-values ((simple-array single-float (768 *)))
             (values (simple-array single-float (768)) &optional))
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

(serapeum:-> fit-pca ((util:fixed-entries 768) (single-float 0.0 1.0))
             (values magicl:matrix/single-float (simple-array single-float (768)) &optional))
(defun fit-pca (descriptors explained-variance)
  (declare (optimize (speed 3)))
  (let* ((transposed (util:transpose-2d descriptors))
         (means (mean-values transposed)))
    (util:loop-array (transposed (i j))
      (let ((mean (aref means i)))
        (setf (aref transposed i j)
              (- (aref transposed i j) mean))))
    (multiple-value-bind (u s vt)
        (magicl:svd
         (magicl:make-tensor
          'magicl:matrix/single-float
          (array-dimensions descriptors)
          :layout :column-major
          :storage (sb-ext:array-storage-vector transposed))
         :reduced t)
      (declare (ignore u))
      (let* ((s (make-array 768
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
                                   0))))
        (values
         (magicl:slice vt '(0 0) (list n-components 768))
         means)))))

(serapeum:-> transform-pca
             ((util:fixed-entries 768)
              magicl:matrix/single-float
              (simple-array single-float (768)))
             (values (simple-array single-float (* *)) &optional))
(defun transform-pca (descriptors vt means)
  (declare (optimize (speed 3)))
  (let ((transposed (util:transpose-2d descriptors)))
    (util:loop-array (transposed (i j))
      (let ((mean (aref means i)))
        (setf (aref transposed i j)
              (- (aref transposed i j) mean))))
    (let ((matrix (magicl:make-tensor
                   'magicl:matrix/single-float
                   (array-dimensions descriptors)
                   :layout :column-major
                   :storage (sb-ext:array-storage-vector transposed))))
      ;; Compute a transposed result to ease column-major to row-major
      ;; conversion
      (let* ((transformed (magicl:mult vt matrix :transb :t))
             ;; Convert back to an ordinary lisp array
             (result (make-array (reverse (magicl:shape transformed))
                                 :element-type 'single-float)))
        (let ((tr (magicl::storage transformed)))
          (declare (type (simple-array single-float) tr))
          (loop for i below (array-total-size result) do
                (setf (row-major-aref result i)
                      (aref tr i))))
        result))))

(serapeum:-> invert-pca
             ((simple-array single-float (* *))
              magicl:matrix/single-float
              (simple-array single-float (768)))
             (values (util:fixed-entries 768) &optional))
(defun invert-pca (pca vt means)
  (declare (optimize (speed 3)))
  ;; Compute a transposed result to ease column-major to row-major
  ;; conversion
  (let* ((matrix (magicl:make-tensor
                  'magicl:matrix/single-float
                  (reverse (array-dimensions pca))
                  :layout :column-major
                  :storage (sb-ext:array-storage-vector pca)))
         (inverted (magicl::storage (magicl:mult vt matrix :transa :t)))
         (result (make-array (list (array-dimension pca 0) 768)
                             :element-type 'single-float)))
    (declare (type (simple-array single-float (*)) inverted))
    (loop for i below (array-total-size result) do
          (setf (row-major-aref result i) (aref inverted i)))
    (util:loop-array (result (i j))
      (incf (aref result i j) (aref means j)))
    result))
