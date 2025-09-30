(defpackage soil-align/preprocessing
  (:use #:cl)
  (:local-nicknames (#:util #:soil-align/util))
  (:export #:image
           #:histogram
           #:clahe
           #:normalize-image))
(in-package :soil-align/preprocessing)

(serapeum:-> histograms
             ((util:image (unsigned-byte 8)) alexandria:positive-fixnum)
             (values (util:histograms (unsigned-byte 64)) &optional))
(defun histograms (image bin-size)
  (declare (optimize (speed 3)))
  (let ((h (array-dimension image 0))
        (w (array-dimension image 1))
        (d (array-dimension image 2)))
    (util:rmvb (((nh rh) (floor h bin-size))
                ((nw rw) (floor w bin-size))
                ((nd rd) (floor d bin-size)))
      ;; SBCL gives shit here
      (unless (every #'zerop (list rh rw rd))
        (error "BIN-SIZE does not divide dimensions"))
      (let ((histograms (make-array (list nh nw nd 256)
                                    :element-type '(unsigned-byte 64)
                                    :initial-element 0)))
        (util:loop-array (image (i j k))
          (let ((bh (floor i bin-size))
                (bw (floor j bin-size))
                (bd (floor k bin-size)))
            (incf (aref histograms bh bw bd (aref image i j k)))))
        histograms))))

(serapeum:-> histograms->cdfs ((util:histograms (unsigned-byte 64)))
             (values (util:histograms single-float) &optional))
(defun histograms->cdfs (histograms)
  (declare (optimize (speed 3)))
  (let ((cdfs            (make-array (array-dimensions histograms)
                                     :element-type '(unsigned-byte 64)))
        (normalized-cdfs (make-array (array-dimensions histograms)
                                     :element-type 'single-float)))
    (util:loop-ranges ((i 0 (array-dimension histograms 0))
                       (j 0 (array-dimension histograms 1))
                       (k 0 (array-dimension histograms 2)))
      (let ((idx (array-row-major-index histograms i j k 0)))
        (setf (row-major-aref cdfs idx) (row-major-aref histograms idx))
        (loop for l from 1 below 256
              for %idx = (+ idx l) do
              (setf (row-major-aref cdfs %idx)
                    (+ (row-major-aref cdfs (1- %idx))
                       (row-major-aref histograms %idx))))
        (loop with max = (float (row-major-aref cdfs (+ idx 255)))
              for l below 256
              for %idx = (+ idx l) do
              (setf (row-major-aref normalized-cdfs %idx)
                    (/ (row-major-aref cdfs %idx) max)))))
    normalized-cdfs))

(declaim (inline clamp))
(defun clamp (x min max)
  (min (max x min) max))

(serapeum:-> histogram-row-major-index
             ((util:histograms *) fixnum fixnum fixnum)
             (values alexandria:non-negative-fixnum &optional))
(declaim (inline histogram-row-major-index))
(defun histogram-row-major-index (histogram i j k)
  (let ((i (clamp i 0 (1- (array-dimension histogram 0))))
        (j (clamp j 0 (1- (array-dimension histogram 1))))
        (k (clamp k 0 (1- (array-dimension histogram 2)))))
    (array-row-major-index histogram i j k 0)))

(serapeum:-> clip-histogram!
             ((util:histograms (unsigned-byte 64))
              alexandria:positive-fixnum
              alexandria:non-negative-fixnum
              (single-float 0.0 1.0))
             (values &optional))
(defun clip-histogram! (histograms bin-size index clip-limit)
  (declare (optimize (speed 3)))
  (let* ((clip-value (floor (* clip-limit (expt bin-size 3))))
         (clipped-sum
          (loop for i below 256
                for idx = (+ index i)
                for x   = (row-major-aref histograms idx)
                ;; TODO: Add a constraint to x in sbcl-float-features
                for x-clipped = (min x clip-value)
                for residue   = (- x x-clipped)
                do (setf (row-major-aref histograms idx) x-clipped)
                sum residue of-type (unsigned-byte 64))))
    (declare (type fixnum clip-value))
    (loop with increment = (floor clipped-sum 256)
          for i below 256
          for idx = (+ index i) do
          (incf (row-major-aref histograms idx) increment))
    ;; 5 in each bin
    (if (< clipped-sum (* 256 5))
        (values)
        (clip-histogram! histograms bin-size index clip-limit))))

(serapeum:-> clip-histograms!
             ((util:histograms (unsigned-byte 64))
              alexandria:positive-fixnum
              (single-float 0.0 1.0))
             (values (util:histograms (unsigned-byte 64)) &optional))
(defun clip-histograms! (histograms bin-size clip-limit)
  (declare (optimize (speed 3)))
  (util:loop-ranges ((i 0 (array-dimension histograms 0))
                     (j 0 (array-dimension histograms 1))
                     (k 0 (array-dimension histograms 2)))
    (clip-histogram!
     histograms bin-size
     (array-row-major-index histograms i j k 0)
     clip-limit))
  histograms)

(serapeum:-> clahe-transform-pixel
             ((util:histograms single-float)
              (unsigned-byte 8)
              alexandria:positive-fixnum
              alexandria:non-negative-fixnum
              alexandria:non-negative-fixnum
              alexandria:non-negative-fixnum)
             (values single-float &optional))
(defun clahe-transform-pixel (table v bin-size i j k)
  (declare (optimize (speed 3)))
  (flet ((access-pixel (i j k)
           (let ((index (histogram-row-major-index table i j k)))
             (row-major-aref table (+ index v)))))
    (declare (inline access-pixel))
    (let* ((bin-size (float bin-size))
           (half-bin-size (/ bin-size 2)))
      (util:interpolate #'access-pixel
                        (- i half-bin-size)
                        (- j half-bin-size)
                        (- k half-bin-size)
                        bin-size))))

(serapeum:-> default-bin-size ((util:image *))
             (values alexandria:positive-fixnum &optional))
(defun default-bin-size (image)
  (let ((side (min (array-dimension image 0)
                   (array-dimension image 1)
                   (array-dimension image 2))))
    (nth-value
     0 (floor side 8))))

;; https://en.wikipedia.org/wiki/Adaptive_histogram_equalization
(serapeum:-> clahe ((util:image (unsigned-byte 8)) &key
                    (:bin-size   alexandria:positive-fixnum)
                    (:clip-limit (single-float 0.0 1.0)))
             (values (util:image single-float) &optional))
(defun clahe (image &key (bin-size (default-bin-size image)) (clip-limit 0.015))
  "Perform adaptive histogram equalization (constrast enhancement) of an image."
  (declare (optimize (speed 3)))
  (let ((cdf (histograms->cdfs
              (clip-histograms!
               (histograms image bin-size) bin-size clip-limit)))
        (result (make-array (array-dimensions image) :element-type 'single-float)))
    (util:loop-array (result (i j k))
      (setf (aref result i j k)
            (clahe-transform-pixel cdf (aref image i j k) bin-size i j k)))
    result))

(serapeum:-> normalize-image ((util:image (unsigned-byte 8)))
             (values (util:image single-float) &optional))
(defun normalize-image (image)
  (declare (optimize (speed 3)))
  (let ((result (make-array (array-dimensions image) :element-type 'single-float)))
    (loop for i below (array-total-size result) do
          (setf (row-major-aref result i)
                (/ (row-major-aref image i) 255.0)))
    result))
