(defpackage soil-align/transform
  (:use #:cl)
  (:local-nicknames (#:ff   #:float-features))
  (:export #:affine-transform))
(in-package :soil-align/transform)

;; ========================================
;; Faster replacements for magick functions
;; ========================================

(deftype affine-transform () '(simple-array single-float (4 4)))

(serapeum:-> select-rows (magicl:matrix/single-float list)
             (values magicl:matrix/single-float &optional))
(defun select-rows (m is)
  (declare (optimize (speed 3)))
  (let* ((length (length is))
         (n (second (magicl:shape m)))
         (result (magicl:make-tensor
                  'magicl:matrix/single-float
                  (list length n))))
    (loop for i below length
          for ridx in is do
          (loop for j fixnum below n do
                (setf (magicl:tref result i j)
                      (magicl:tref m ridx j))))
    result))

;; MAGICL:VSTACK is extremely slow
(serapeum:-> vstack (list)
             (values magicl:matrix/single-float &optional))
(defun vstack (list)
  (declare (optimize (speed 3)))
  (let* ((length (length list))
         (n (second (magicl:shape (first list))))
         (result (magicl:make-tensor
                  'magicl:matrix/single-float
                  (list length n))))
    (loop for i below length
          for row in list do
          (loop for j fixnum below n do
                (setf (magicl:tref result i j)
                      (magicl:tref row 0 j))))
    result))

;; And so is MAGICL:ROW
(serapeum:-> row (magicl:matrix/single-float
                  alexandria:non-negative-fixnum)
             (values magicl:matrix/single-float &optional))
(defun row (m idx)
  (declare (optimize (speed 3)))
  (let* ((n (second (magicl:shape m)))
         (result (magicl:make-tensor
                  'magicl:matrix/single-float
                  (list 1 n))))
    (loop for i fixnum below n do
          (setf (magicl:tref result 0 i)
                (magicl:tref m idx i)))
    result))

;; ============
;; Ransac stuff
;; ============

;; Convert a list of keypoint pairs (matches) into 2 Nx4 matrices
;; where the first matrix corresponds to the first keypoint in a pair
;; and the second matrix corresponds to the second keypoint in a pair.
(serapeum:-> matches->matrices (list)
             (values magicl:matrix/single-float magicl:matrix/single-float &optional))
(defun matches->matrices (matches)
  (flet ((coord-list (kp)
           (list (aref kp 0) (aref kp 1) (aref kp 2) 1f0)))
    (multiple-value-bind (xs ys n)
        (loop for (kp1 . kp2) in matches
              append (coord-list kp1) into xs
              append (coord-list kp2) into ys
              sum 1 into n
              finally (return (values xs ys n)))
      (values
       (magicl:from-list xs (list n 4))
       (magicl:from-list ys (list n 4))))))

;; Return a matrix βs so that ys ≈ xs * βs using least squares.
(serapeum:-> least-squares-fit
             (magicl:matrix/single-float magicl:matrix/single-float)
             (values magicl:matrix/single-float &optional))
(defun least-squares-fit (xs ys)
  (or
   (ignore-errors
     (magicl:mult
      (magicl:mult
       (magicl:inv (magicl:mult xs xs :transa :t))
       xs :transb :t)
      ys))
   (magicl:eye '(4 4) :type 'single-float)))


(serapeum:-> fit-error (magicl:matrix/single-float
                        magicl:matrix/single-float
                        magicl:matrix/single-float)
             (values single-float &optional))
(defun fit-error (βs xs ys)
  (let ((diff (magicl:.- ys (magicl:@ xs βs))))
    (magicl:norm (magicl:reshape diff (list (magicl:size diff))))))

(defun random-integers (k n)
  "Collect K random integer from 0 (inclusive) to N (exclusive)
without repetitions."
  (labels ((%go (acc k)
             (if (zerop k) acc
                 (let ((x (random n)))
                   (if (find x acc :test #'=)
                       (%go acc k)
                       (%go (cons x acc) (1- k)))))))
    (%go nil k)))

(serapeum:-> ransac-iteration (magicl:matrix/single-float
                               magicl:matrix/single-float
                               alexandria:positive-fixnum
                               alexandria:positive-fixnum
                               (single-float 0f0)
                               (single-float 0f0))
         (values boolean &optional
                 magicl:matrix/single-float
                 (single-float 0f0)
                 alexandria:positive-fixnum))
(defun ransac-iteration (xs ys k inliers ε prev-error)
  "Perform one iteration of RANSAC fit, namely find a linear model ΒS
so that ΒS(XS) fits YS. K is the number of points to find an initial
fit. INLIERS is the number of inliers which is necassary to treat the
model as good. ε is a criterion for being an inlier, namely |Y -
ΒS(X)| must be less that ε. PREV-ERROR is the fit error from the
previous step."
  (let* ((length (first (magicl:shape xs)))
         (is (random-integers k length))
         (%xs (select-rows xs is))
         (%ys (select-rows ys is))
         (βs (least-squares-fit %xs %ys)))
    (multiple-value-bind (n xs ys)
        (loop for i below length
              for xrow = (row xs i)
              for yrow = (row ys i)
              for yfit = (magicl:mult xrow βs)
              for pair-err = (magicl:norm (magicl:.- yrow yfit))
              when (< pair-err ε)
              collect xrow into fit-x-rows and
              collect yrow into fit-y-rows and
              sum 1 into n
              finally (when (not (zerop n))
                        (return
                          (values
                           n (vstack fit-x-rows) (vstack fit-y-rows)))))
      (when n
        (let* ((βs (least-squares-fit xs ys))
               (fit-error (fit-error βs xs ys)))
          (when (and (>= n inliers) (< fit-error prev-error))
            (values t βs fit-error n)))))))

(serapeum:-> ransac-fit (magicl:matrix/single-float
                         magicl:matrix/single-float
                         alexandria:positive-fixnum
                         alexandria:positive-fixnum
                         alexandria:positive-fixnum
                         (single-float 0f0))
             (values (or magicl:matrix/single-float null)
                     single-float alexandria:positive-fixnum &optional))
(defun ransac-fit (xs ys max-iter k min-inliers err)
  (labels ((%go (best-fit best-err inliers n)
             (if (zerop n)
                 (values best-fit best-err inliers)
                 (multiple-value-bind (successp fit %err %inliers)
                     (ransac-iteration xs ys k min-inliers err best-err)
                   (let ((n (1- n)))
                     (if successp
                         (%go fit %err %inliers n)
                         (%go best-fit best-err inliers n)))))))
    (%go nil ff:single-float-positive-infinity min-inliers max-iter)))

(serapeum:-> matrix->array (magicl:matrix/single-float)
             (values affine-transform &optional))
(defun matrix->array (m)
  (let ((res (make-array '(4 4) :element-type 'single-float)))
    (loop for i below 4 do
          (loop for j below 4 do
                (setf (aref res i j)
                      (magicl:tref m i j))))
    res))

(serapeum:-> affine-transform (list &key
                                    (:max-iter    alexandria:positive-fixnum)
                                    (:seed-points alexandria:positive-fixnum)
                                    (:min-inliers (single-float 0f0 1f0))
                                    (:err         (single-float 0f0)))
         (values (or null affine-transform)
                 single-float alexandria:positive-fixnum &optional))
(defun affine-transform (matches
                         &key (min-inliers 8f-1) (max-iter 500) (seed-points 15) (err 100f0))
  "Find an affine transform matrix which transforms the first keypoint
in each pair of matches to the second keypoint. Keypoint parameters
are related to the RANSAC algorithm: @c(MAX-ITER) is the maximal
number of iterations, @c(SEED-POINTS) is an initial number of points
to make a fit. A parameter @c(MIN-INLIERS) controls a minimal ratio of
inliers to treat a fit as successful. A point is well-fit if
 \\(\\| y - Ax \\|\\) is less than @c(ERR), (\\(A\\) is a candidate
for the found fit)."
  (multiple-value-bind (fit error inliers)
      (multiple-value-bind (xs ys)
          (matches->matrices matches)
        (ransac-fit xs ys max-iter seed-points (floor (* (length matches) min-inliers)) err))
    (values (if fit (matrix->array (magicl:transpose fit))) error inliers)))
