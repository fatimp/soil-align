(defpackage soil-align/util
  (:use #:cl)
  (:export #:loop-array
           #:loop-ranges
           #:rmvb
           #:image
           #:histograms
           #:descriptor
           #:coordinate
           #:+descriptor-offset+
           #:+descriptor-length+
           #:ffi-error
           #:transpose
           #:interpolate))
(in-package :soil-align/util)

(define-condition ffi-error (error)
  ((message :reader  ffi-error-message
            :initarg :message))
  (:report (lambda (c s)
             (format s "FFI error: ~a" (ffi-error-message c)))))

(defconstant +descriptor-offset+ 3)
(defconstant +descriptor-length+ (- 771 +descriptor-offset+))
(deftype image       (type) `(simple-array ,type 3))
(deftype histograms  (type) `(simple-array ,type (* * * 256)))
(deftype descriptor  ()     '(simple-array single-float (771)))
(deftype coordinate  ()     '(simple-array single-float (3)))

;; Useful macros for iteration which supersede nested loops
(defmacro loop-array ((array indices) &body body)
  (car
   (reduce
    (lambda (entry acc)
      (destructuring-bind (d . idx)
          entry
        `((loop for ,idx below (array-dimension ,array ,d) do
                ,@acc))))
    (loop for idx in indices
          for d from 0 by 1
          collect (cons d idx))
    :from-end t
    :initial-value body)))

(defmacro loop-ranges (specs &body body)
  (car
   (reduce
    (lambda (spec acc)
      (destructuring-bind (idx start end)
          spec
        `((loop for ,idx fixnum from ,start below ,end do
                ,@acc))))
    specs
    :from-end t
    :initial-value body)))

;; Really multiple value bind
(defmacro rmvb (forms &body body)
  (car
   (reduce
    (lambda (form acc)
      (destructuring-bind (variables expression)
          form
        `((multiple-value-bind ,variables
              ,expression
            ,@acc))))
    forms
    :from-end t
    :initial-value body)))

(serapeum:-> transpose ((image single-float))
             (values (image single-float) &optional))
(defun transpose (array)
  (declare (optimize (speed 3)))
  (let ((result (make-array (reverse (array-dimensions array))
                            :element-type 'single-float)))
    (loop-array (result (i j k))
     (setf (aref result i j k)
           (aref array k j i)))
    result))

;; Linear interpolation

(declaim (inline interp1d))
(defun interp1d (v0 v1 x)
  (+ v0 (* (- v1 v0) x)))

(serapeum:-> interpolate
             ((serapeum:-> (fixnum fixnum fixnum)
                           (values single-float &optional))
              real real real
              (real (0)) (real (0)) (real (0)))
             (values single-float &optional))
(declaim (inline interpolate))
(defun interpolate (f x y z divisor-x divisor-y divisor-z)
  "Interpolate F in the point (X/DIVISOR-X, Y/DIVISOR-Y, Z/DIVIZOR-Z)."
  (let ((divisor-x (float divisor-x))
        (divisor-y (float divisor-y))
        (divisor-z (float divisor-z)))
    (rmvb (((qi ri) (floor x divisor-x))
           ((qj rj) (floor y divisor-y))
           ((qk rk) (floor z divisor-z)))
            ;; For code formatting
      (flet ((id (x) x))
        (declare (inline id))
        (let* ((ri (/ ri divisor-x))
               (rj (/ rj divisor-y))
               (rk (/ rk divisor-z))

               (v000 (funcall f (id qi) (id qj) (id qk)))
               (v001 (funcall f (id qi) (id qj) (1+ qk)))
               (v010 (funcall f (id qi) (1+ qj) (id qk)))
               (v011 (funcall f (id qi) (1+ qj) (1+ qk)))
               (v100 (funcall f (1+ qi) (id qj) (id qk)))
               (v101 (funcall f (1+ qi) (id qj) (1+ qk)))
               (v110 (funcall f (1+ qi) (1+ qj) (id qk)))
               (v111 (funcall f (1+ qi) (1+ qj) (1+ qk)))

               (v00 (interp1d v000 v001 rk))
               (v01 (interp1d v010 v011 rk))
               (v10 (interp1d v100 v101 rk))
               (v11 (interp1d v110 v111 rk))

               (v0 (interp1d v00 v01 rj))
               (v1 (interp1d v10 v11 rj))

               (v (interp1d v0 v1 ri)))
          v)))))
