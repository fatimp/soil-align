(defpackage soil-align/transform
  (:use #:cl)
  (:local-nicknames (#:ff   #:float-features)
                    (#:em   #:entzauberte-matrices)
                    (#:util #:soil-align/util))
  (:export #:rigid-transform
           #:affine-transform))
(in-package :soil-align/transform)

(deftype coordinate       () '(simple-array single-float (3)))
(deftype affine-transform () '(simple-array single-float (4 4)))

(alexandria:define-constant +flip-det+
    (make-array '(3 3)
                :element-type 'single-float
                :initial-contents '((1.0 0.0  0.0)
                                    (0.0 1.0  0.0)
                                    (0.0 0.0 -1.0)))
  :test #'equalp)

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
  (let ((result (make-array '(4 4)
                            :element-type 'single-float
                            :initial-element 0.0)))
    (loop for i below 3 do
      (loop for j below 3 do
        (setf (aref result i j)
              (aref rot i j))))
    (setf (aref result 3 3) 1.0)
    result))

(declaim (inline affine-translation))
(defun affine-translation (trans)
  "Convert a translation vector to an affine transform."
  (let ((result (make-array '(4 4)
                            :element-type 'single-float
                            :initial-element 0.0)))
    (loop for i below 4 do
      (setf (aref result i i) 1.0))
    (loop for i below 3 do
          (setf (aref result i 3)
                (aref trans i)))
    result))

(serapeum:-> model-fit (list)
             (values affine-transform &optional))
(defun model-fit (matches)
  "Find a constrained affine transform which fits the matches."
  (declare (optimize (speed 3)))
  (let ((q (mapcar #'car matches))
        (p (mapcar #'cdr matches)))
    (serapeum:mvlet ((q c1 (make-matrix q))
                     (p c2 (make-matrix p)))
      (multiple-value-bind (u s vt)
          (em:svd (em:mult p q :ta t))
        (declare (ignore s))
        (let* ((uvt (em:mult u vt))
               (rot (if (> (em:det uvt) 0) uvt
                        (em:mult u (em:mult +flip-det+ vt)))))
            (em:mult (affine-translation c2)
                     (em:mult
                      (affine-rotation rot)
                      (em:invert (affine-translation c1)))))))))

(serapeum:-> to-affine-vector (coordinate)
             (values (simple-array single-float (4)) &optional))
(declaim (inline to-affine-vector))
(defun to-affine-vector (v)
  (let ((av (make-array 4 :element-type 'single-float)))
    (replace av v)
    (setf (aref av 3) 1.0)
    av))

(serapeum:-> match-fit-error (affine-transform (cons coordinate coordinate))
             (values single-float &optional))
(defun match-fit-error (fit match)
  "Calculate an error of the fit for a pair of coordinates."
  (declare (optimize (speed 3)))
  (let ((v1 (to-affine-vector (car match)))
        (v2 (to-affine-vector (cdr match))))
    (em:norm
     (em:sub
      (em:column (em:mult fit (em:vector->column v1)) 0)
      v2))))

(serapeum:-> fit-error (affine-transform list)
             (values single-float &optional))
(defun fit-error (fit matches)
  "Calculate the total fit error."
  (declare (optimize (speed 3)))
  (sqrt
   (loop for (p1 . p2) in matches sum
         (let ((v1 (to-affine-vector p1))
               (v2 (to-affine-vector p2)))
           (let ((diff (em:sub (em:column
                                (em:mult fit (em:vector->column v1)) 0)
                               v2)))
             (em:dot diff diff)))
         single-float)))

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
                 affine-transform
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

(serapeum:-> rigid-transform
             (list &key
                   (:max-iter    alexandria:positive-fixnum)
                   (:seed-points alexandria:positive-fixnum)
                   (:err         (single-float 0f0)))
             (values (or null affine-transform)
                     single-float alexandria:positive-fixnum &optional))
(defun rigid-transform (matches &key (max-iter 500) (seed-points 15) (err 100f0))
    "Find a rigid transform (which means rotation + translation) matrix
which transforms the first keypoint in each pair of matches to the
second keypoint. Keypoint parameters are related to the RANSAC
algorithm: @c(MAX-ITER) is the maximal number of iterations,
@c(SEED-POINTS) is an initial number of points to make a fit. A point
is well-fit if \\(\\| y - Ax \\|\\) is less than @c(ERR), (\\(A\\) is
a candidate for the found fit)."
  (declare (optimize (speed 3)))
  (labels ((%go (best-fit best-err best-inliers n)
             (declare (type alexandria:non-negative-fixnum n))
             (if (zerop n)
                 (values best-fit best-err best-inliers)
                 (multiple-value-bind (successp fit %err %inliers)
                     (ransac-iteration matches seed-points best-inliers err best-err)
                   (let ((n (1- n)))
                     (if successp
                         (%go fit %err %inliers n)
                         (%go best-fit best-err best-inliers n)))))))
    (let ((initial-error ff:single-float-positive-infinity))
      (if (< (length matches) seed-points)
          (values nil initial-error 1)
          (%go    nil initial-error 1 max-iter)))))
