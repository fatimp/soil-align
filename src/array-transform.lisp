(defpackage soil-align/array-transform
  (:use #:cl)
  (:local-nicknames (#:util #:soil-align/util)
                    (#:tran #:soil-align/transform))
  (:export #:apply-transform))
(in-package :soil-align/array-transform)

(serapeum:-> apply-transform-xs
             (tran:affine-transform single-float single-float single-float)
             (values single-float single-float single-float &optional))
(defun apply-transform-xs (m x y z)
  (declare (optimize (speed 3)))
  (let ((x (+ (* (aref m 0 0) x) (* (aref m 0 1) y) (* (aref m 0 2) z) (aref m 0 3)))
        (y (+ (* (aref m 1 0) x) (* (aref m 1 1) y) (* (aref m 1 2) z) (aref m 1 3)))
        (z (+ (* (aref m 2 0) x) (* (aref m 2 1) y) (* (aref m 2 2) z) (aref m 2 3))))
    (values x y z)))

(serapeum:-> apply-transform
             ((util:image (unsigned-byte 8))
              tran:affine-transform
              list &key
              (:nthreads   alexandria:positive-fixnum)
              (:background single-float))
             (values (util:image (unsigned-byte 8)) &optional))
(defun apply-transform (array m shape &key (nthreads 1) (background 0.0))
  "Apply affine transform @c(M) (in the form of 4x4 matrix) to an
image. The result has the shape @c(shape)."
  (declare (optimize (speed 3)))
  (let ((result (make-array shape :element-type '(unsigned-byte 8))))
    (declare (type (util:image (unsigned-byte 8)) result))
    (util:loop-array (result (i j k) :nthreads nthreads)
      (multiple-value-bind (x y z)
          (apply-transform-xs m (float i) (float j) (float k))
        (setf (aref result i j k)
              (round
               (util:interpolate
                (lambda (i j k)
                  (if (array-in-bounds-p array i j k)
                      (float (aref array i j k)) background))
                x y z
                1 1 1)))))
    result))
