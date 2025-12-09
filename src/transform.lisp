(defpackage soil-align/transform
  (:use #:cl)
  (:local-nicknames (#:ff   #:float-features)
                    (#:util #:soil-align/util))
  (:export #:rigid-transform
           #:affine-transform))
(in-package :soil-align/transform)

(deftype coordinate       () '(simple-array single-float (3)))
(deftype affine-transform () '(simple-array single-float (4 4)))

;; =============
;; Model fitting
;; =============

(serapeum:-> center (list) (values coordinate &optional))
(defun center (coords)
  (declare (optimize (speed 3)))
  (let ((length (length coords))
        (state (vector-sum:sum-state 0f0)))
    (labels ((%go (state-x state-y state-z coords)
               (if (null coords)
                   (make-array 3
                               :element-type 'single-float
                               :initial-contents
                               (list (/ (vector-sum:state-sum state-x) length)
                                     (/ (vector-sum:state-sum state-y) length)
                                     (/ (vector-sum:state-sum state-z) length)))
                   (let ((coord (car coords)))
                     (declare (type coordinate coord))
                     (%go (vector-sum:add state-x (aref coord 0))
                          (vector-sum:add state-y (aref coord 1))
                          (vector-sum:add state-z (aref coord 2))
                          (cdr coords))))))
      (%go state state state coords))))

(serapeum:-> make-matrix (list)
             (values (util:fixed-entries 3) coordinate &optional))
(defun make-matrix (coords)
  "Convert a list of coordinates to a Nx3 matrix + 3-vector centroid."
  (let* ((center (center coords))
         (length (length coords))
         (matrix (make-array (list length 3) :element-type 'single-float)))
    (loop for i below length
          for coord of-type coordinate in coords do
          (loop for j below 3 do
                (setf (aref matrix i j)
                      (- (aref coord j)
                         (aref center j)))))
    (values matrix center)))

(declaim (inline affine-rotation))
(defun affine-rotation (rot)
  "Convert a 3x3 rotation matrix to an affine transform."
  (let ((result (magicl:eye '(4 4) :type 'single-float)))
    (loop for i below 3 do
          (loop for j below 3 do
                (setf (magicl:tref result i j)
                      (magicl:tref rot i j))))
    result))

(declaim (inline affine-translation))
(defun affine-translation (trans)
  "Convert a translation vector to an affine transform."
  (let ((result (magicl:eye '(4 4) :type 'single-float)))
    (loop for i below 3 do
          (setf (magicl:tref result i 3)
                (aref trans i)))
    result))

(serapeum:-> model-fit (list)
             (values magicl:matrix/single-float &optional))
(defun model-fit (matches)
  "Find a constrained affine transform which fits the matches."
  (let ((q (mapcar #'car matches))
        (p (mapcar #'cdr matches)))
    (serapeum:mvlet ((q c1 (make-matrix q))
                     (p c2 (make-matrix p)))
      (let ((q (magicl:from-array q (array-dimensions q)))
            (p (magicl:from-array p (array-dimensions p))))
        (multiple-value-bind (u s vt)
            (magicl:svd (magicl:mult p q :transa :t))
          (declare (ignore s))
          (let* ((uvt (magicl:mult u vt))
                 (rot (if (> (magicl:det uvt) 0) uvt
                          (magicl:@ u (magicl:from-diag '(1.0 1.0 -1.0)) vt))))
            (magicl:@ (affine-translation c2)
                      (affine-rotation rot)
                      (magicl:inv (affine-translation c1)))))))))

(serapeum:-> match-fit-error (magicl:matrix/single-float
                              (cons coordinate coordinate))
             (values single-float &optional))
(defun match-fit-error (fit match)
  "Calculate an error of the fit for a pair of coordinates."
  (let* ((p1 (car match))
         (p2 (cdr match))
         (c1 (magicl:ones '(4) :type 'single-float))
         (c2 (magicl:ones '(4) :type 'single-float)))
    (loop for i below 3 do
          (setf (magicl:tref c1 i) (aref p1 i)
                (magicl:tref c2 i) (aref p2 i)))
    (magicl:norm (magicl:.- (magicl:@ fit c1) c2))))

(serapeum:-> fit-error (magicl:matrix/single-float list)
             (values single-float &optional))
(defun fit-error (fit matches)
  "Calculate the total fit error."
  (sqrt
   (loop for (p1 . p2) in matches sum
         (let ((c1 (magicl:ones '(4) :type 'single-float))
               (c2 (magicl:ones '(4) :type 'single-float)))
           (loop for i below 3 do
                 (setf (magicl:tref c1 i) (aref p1 i)
                       (magicl:tref c2 i) (aref p2 i)))
           (let ((diff (magicl:.- (magicl:@ fit c1) c2)))
             (magicl:dot diff diff))))))

;; ============
;; Ransac stuff
;; ============

(defun random-integers (k n)
  "Collect K random integer from 0 (inclusive) to N (exclusive)
without repetitions."
  (assert (>= n k))
  (labels ((%go (acc k)
             (if (zerop k) acc
                 (let ((x (random n)))
                   (if (find x acc :test #'=)
                       (%go acc k)
                       (%go (cons x acc) (1- k)))))))
    (%go nil k)))

(defun select-entries (list indices)
  "Select items with specific indices from a list."
  (let ((indices (sort (copy-list indices) #'<)))
    (labels ((%go (list indices acc i)
               (cond
                 ((null indices) acc)
                 ((null list)
                  (error "The end of LIST is reached"))
                 ((= i (car indices))
                  (%go (cdr list) (cdr indices) (cons (car list) acc) (1+ i)))
                 (t
                  (%go (cdr list) indices acc (1+ i))))))
      (%go list indices nil 0))))

(serapeum:-> ransac-iteration
             (list
              alexandria:positive-fixnum
              alexandria:positive-fixnum
              (single-float 0f0)
              (single-float 0f0))
         (values boolean &optional
                 magicl:matrix/single-float
                 (single-float 0f0)
                 alexandria:positive-fixnum))
(defun ransac-iteration (matches k ninliers ε prev-error)
  "Perform one iteration of RANSAC fit, namely find a linear model ΒS
so that ΒS(XS) fits YS. K is the number of points to find an initial
fit. NINLIERS is the number of inliers which is necassary to treat the
model as good. ε is a criterion for being an inlier, namely |Y -
ΒS(X)| must be less that ε. PREV-ERROR is the fit error from the
previous step."
  (let* ((length (length matches))
         (indices (random-integers k length))
         (subset (select-entries matches indices))
         (fit (model-fit subset))
         (inliers
          (loop for match in matches
                for err = (match-fit-error fit match)
                when (< err ε)
                collect match)))
    (when inliers
      (let* ((fit (model-fit inliers))
             (err (fit-error fit inliers))
             (n (length inliers)))
        (when (or (> n ninliers)
                  (and (= n ninliers) (< err prev-error)))
          (values t fit err n))))))

(serapeum:-> ransac-fit
             (list
              alexandria:positive-fixnum
              alexandria:positive-fixnum
              alexandria:positive-fixnum
              (single-float 0f0))
             (values (or magicl:matrix/single-float null)
                     single-float alexandria:positive-fixnum &optional))
(defun ransac-fit (matches max-iter k min-inliers err)
  (labels ((%go (best-fit best-err best-inliers n)
             (if (zerop n)
                 (values best-fit best-err best-inliers)
                 (multiple-value-bind (successp fit %err %inliers)
                     (ransac-iteration matches k best-inliers err best-err)
                   (let ((n (1- n)))
                     (if successp
                         (%go fit %err %inliers n)
                         (%go best-fit best-err best-inliers n)))))))
    (let ((initial-error ff:single-float-positive-infinity))
      (if (< (length matches) k)
          (values nil initial-error min-inliers)
          (%go    nil initial-error min-inliers max-iter)))))

(serapeum:-> matrix->array (magicl:matrix/single-float)
             (values affine-transform &optional))
(defun matrix->array (m)
  (let ((res (make-array '(4 4) :element-type 'single-float)))
    (loop for i below 4 do
          (loop for j below 4 do
                (setf (aref res i j)
                      (magicl:tref m i j))))
    res))

(serapeum:-> rigid-transform
             (list &key
                   (:max-iter    alexandria:positive-fixnum)
                   (:seed-points alexandria:positive-fixnum)
                   (:min-inliers (single-float 0f0 1f0))
                   (:err         (single-float 0f0)))
             (values (or null affine-transform)
                     single-float alexandria:positive-fixnum &optional))
(defun rigid-transform (matches
                        &key (min-inliers 6f-1) (max-iter 500) (seed-points 15) (err 100f0))
  "Find a rigid transform (which means rotation + translation) matrix
which transforms the first keypoint in each pair of matches to the
second keypoint. Keypoint parameters are related to the RANSAC
algorithm: @c(MAX-ITER) is the maximal number of iterations,
@c(SEED-POINTS) is an initial number of points to make a fit. A
parameter @c(MIN-INLIERS) controls a minimal ratio of inliers to treat
a fit as successful. A point is well-fit if \\(\\| y - Ax \\|\\) is
less than @c(ERR), (\\(A\\) is a candidate for the found fit)."
  (multiple-value-bind (fit error inliers)
      (ransac-fit matches max-iter seed-points (floor (* (length matches) min-inliers)) err)
    (values (if fit (matrix->array fit)) error inliers)))
