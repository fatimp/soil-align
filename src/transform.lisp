(defpackage soil-align/transform
  (:use #:cl)
  (:local-nicknames (#:ff   #:float-features)
                    (#:em   #:entzauberte-matrices)
                    (#:util #:soil-align/util))
  (:export #:rigid-transform-fit
           #:rigid+scaling-transform-fit
           #:ransac
           #:affine-transform))
(in-package :soil-align/transform)

(deftype coordinate       () '(simple-array single-float (3)))
(deftype affine-transform () '(simple-array single-float (4 4)))

(deftype fit-function ()
  '(function (list)
    (values affine-transform &optional)))

(alexandria:define-constant +flip-det+
    (make-array '(3 3)
                :element-type 'single-float
                :initial-contents '((1.0 0.0  0.0)
                                    (0.0 1.0  0.0)
                                    (0.0 0.0 -1.0)))
  :test #'equalp)

;; ======================
;; Transform constructors
;; ======================

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

(declaim (inline affine-uniform-scaling))
(defun affine-uniform-scaling (s)
  "Convert uniform scaling to an affine transform"
  (let ((result (make-array '(4 4)
                            :element-type 'single-float
                            :initial-element 0.0)))
    (loop for i below 3 do
      (setf (aref result i i) s))
    (setf (aref result 3 3) 1.0)
    result))

;; ==============================
;; Determine centroid and scaling
;; ==============================

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

(serapeum:-> uniform-scale (list coordinate)
             (values single-float &optional))
(defun uniform-scale (coords center)
  (declare (optimize (speed 3)))
  (let ((length (length coords))
        (state (vector-sum:sum-state 0f0)))
    (labels ((update (state coord idx)
               (vector-sum:add state (expt (- (aref coord  idx)
                                              (aref center idx))
                                           2)))
             (%go (state-x state-y state-z coords)
               (if (null coords)
                   (values (vector-sum:state-sum state-x)
                           (vector-sum:state-sum state-y)
                           (vector-sum:state-sum state-z))
                   (let ((coord (car coords)))
                     (declare (type coordinate coord))
                     (%go (update state-x coord 0)
                          (update state-y coord 1)
                          (update state-z coord 2)
                          (cdr coords))))))
      (declare (inline update))
      (multiple-value-bind (x y z)
          (%go state state state coords)
        (sqrt (/ (+ x y z) length))))))

;; ==========================================
;; Rigid transform + uniform scale (optional)
;; ==========================================

(serapeum:-> make-matrix (boolean list)
             (values (util:fixed-entries 3) coordinate single-float &optional))
(defun make-matrix (scalingp coords)
  "Convert a list of coordinates to a Nx3 matrix + 3-vector centroid +
scaling parameter. The scaling parameter is always 1.0 if @c(scalingp)
is @c(NIL)."
  (declare (optimize (speed 3)))
  (let* ((center (center coords))
         (scale  (if scalingp (uniform-scale coords center) 1.0))
         (length (length coords))
         (matrix (make-array (list length 3) :element-type 'single-float)))
    (loop for i below length
          for coord of-type coordinate in coords do
          (loop for j below 3 do
                (setf (aref matrix i j)
                      (/ (- (aref coord j)
                            (aref center j))
                         scale))))
    (values matrix center scale)))

(serapeum:-> %rigid-transform-fit (boolean list)
             (values affine-transform &optional))
(defun %rigid-transform-fit (scalingp matches)
  "Find a rigid transform which fits the matches. If @c(scalingp) is
@c(T), the transform also includes uniform scaling."
  (declare (optimize (speed 3)))
  (let ((q (mapcar #'car matches))
        (p (mapcar #'cdr matches)))
    (serapeum:mvlet ((q c1 s1 (make-matrix scalingp q))
                     (p c2 s2 (make-matrix scalingp p)))
      (multiple-value-bind (u s vt)
          (em:svd (em:mult p q :ta t))
        (declare (ignore s))
        (let* ((uvt (em:mult u vt))
               (rot (if (> (em:det uvt) 0) uvt
                        (em:@ u +flip-det+ vt))))
            (em:@ (affine-translation c2)
                  (affine-uniform-scaling s2)
                  (affine-rotation rot)
                  (em:invert (affine-uniform-scaling s1))
                  (em:invert (affine-translation c1))))))))

(serapeum:-> rigid-transform-fit (list)
             (values affine-transform &optional))
(defun rigid-transform-fit (matches)
  "Find a rigid transform which fits the matches."
  (%rigid-transform-fit nil matches))

(serapeum:-> rigid+scaling-transform-fit (list)
             (values affine-transform &optional))
(defun rigid+scaling-transform-fit (matches)
  "Find a rigid+scaling transform which fits the matches."
  (%rigid-transform-fit t matches))

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

(serapeum:-> ransac-iteration (fit-function list
                               alexandria:positive-fixnum
                               alexandria:positive-fixnum
                               (single-float 0f0)
                               (single-float 0f0))
             (values boolean &optional
                     affine-transform
                     (single-float 0f0)
                     alexandria:positive-fixnum))
(defun ransac-iteration (f matches k ninliers ε prev-error)
  "Perform one iteration of RANSAC fit, namely find a linear model ΒS
so that ΒS(XS) fits YS. K is the number of points to find an initial
fit. NINLIERS is the number of inliers which is necassary to treat the
model as good. ε is a criterion for being an inlier, namely |Y -
ΒS(X)| must be less that ε. PREV-ERROR is the fit error from the
previous step."
  (let* ((length (length matches))
         (indices (random-integers k length))
         (subset (select-entries matches indices))
         (fit (funcall f subset))
         (inliers
          (loop for match in matches
                for err = (match-fit-error fit match)
                when (< err ε)
                collect match)))
    (when inliers
      (let* ((fit (funcall f inliers))
             (err (fit-error fit inliers))
             (n (length inliers)))
        (when (or (> n ninliers)
                  (and (= n ninliers) (< err prev-error)))
          (values t fit err n))))))

(serapeum:-> ransac (fit-function list &key
                     (:max-iter    alexandria:positive-fixnum)
                     (:seed-points alexandria:positive-fixnum)
                     (:err         (single-float 0f0)))
             (values (or null affine-transform)
                     single-float alexandria:positive-fixnum &optional))
(defun ransac (f matches &key (max-iter 500) (seed-points 15) (err 100f0))
    "Find an affine transform which transforms the first keypoint in
each pair of matches to the second keypoint. Keypoint parameters are
related to the RANSAC algorithm: @c(MAX-ITER) is the maximal number of
iterations, @c(SEED-POINTS) is an initial number of points to make a
fit. A point is well-fit if \\(\\| y - Ax \\|\\) is less than @c(ERR),
(\\(A\\) is a candidate for the found fit). The function @c(F) fits a
subset of @c(MATCHES) and controls the type of transform (e.g. rigid,
translation, etc.)."
  (declare (optimize (speed 3)))
  (labels ((%go (best-fit best-err best-inliers n)
             (declare (type alexandria:non-negative-fixnum n))
             (if (zerop n)
                 (values best-fit best-err best-inliers)
                 (multiple-value-bind (successp fit %err %inliers)
                     (ransac-iteration f matches seed-points best-inliers err best-err)
                   (let ((n (1- n)))
                     (if successp
                         (%go fit %err %inliers n)
                         (%go best-fit best-err best-inliers n)))))))
    (let ((initial-error ff:single-float-positive-infinity))
      (if (< (length matches) seed-points)
          (values nil initial-error 1)
          (%go    nil initial-error 1 max-iter)))))
