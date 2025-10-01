;; B. Rister, M. A. Horowitz and D. L. Rubin, "Volumetric Image
;; Registration From Invariant Keypoints," in IEEE Transactions on
;; Image Processing, vol. 26, no. 10, pp. 4900-4910, Oct. 2017. doi:
;; 10.1109/TIP.2017.2722689

(defpackage soil-align/sift3d
  (:use #:cl)
  (:local-nicknames (#:util #:soil-align/util))
  (:export #:descriptors))
(in-package :soil-align/sift3d)

;; Libraries
(cffi:define-foreign-library libimutil
  (:unix (:or "libimutil.so"))
  (t (:default "libimutil")))

(cffi:define-foreign-library libsift3d
  (:unix (:or "libsift3D.so"))
  (t (:default "libsift3D")))

(cffi:use-foreign-library libimutil)
(cffi:use-foreign-library libsift3D)

;; ==========
;;   Images
;; ==========

(cffi:defcstruct (image :size 104)
  (data (:pointer :float)))

;; Can be safely called after init_im()
(cffi:defcfun ("init_im_with_dims" %init-im-with-dims) :int
  (image (:pointer (:struct image)))
  (nx    :int)
  (ny    :int)
  (nz    :int)
  (nc    :int))

(cffi:defcfun ("init_im" %init-im) :void
  (image (:pointer (:struct image))))

(cffi:defcfun ("im_free" %im-free) :void
  (image (:pointer (:struct image))))

(serapeum:-> array->image (t (simple-array single-float 3))
             (values &optional))
(defun array->image (image array)
  (unless (zerop
           (%init-im-with-dims
            image
            (array-dimension array 0)
            (array-dimension array 1)
            (array-dimension array 2)
            1))
    (error 'util:ffi-error :message "Cannot allocate an image"))
  (let ((data-ptr (cffi:foreign-slot-value image '(:struct image) 'data)))
    (loop for i below (array-total-size array) do
          (setf (cffi:mem-aref data-ptr :float i)
                (row-major-aref array i))))
  (values))

(defmacro with-image ((image array) &body body)
  `(cffi:with-foreign-object (,image '(:struct image))
     (%init-im ,image)
     (unwind-protect
          (progn
            (array->image ,image ,array)
            ,@body)
       (%im-free ,image))))

;; ====================
;;  The main structure
;; ====================

(cffi:defcstruct (sift3d :size 304))

(cffi:defcfun ("init_SIFT3D" %init-sift3d) :int
  (sift3d (:pointer (:struct sift3d))))

(cffi:defcfun ("cleanup_SIFT3D" %cleanup-sift3d) :void
  (sift3d (:pointer (:struct sift3d))))

(defun init-sift3d (sift3d)
  (unless (zerop (%init-sift3d sift3d))
    (error 'util:ffi-error :message "Cannot allocate SIFT3D object")))

(defmacro with-sift3d ((sift3d) &body body)
  `(cffi:with-foreign-object (,sift3d '(:struct sift3d))
     (init-sift3d ,sift3d)
     (unwind-protect
          (progn
            ,@body)
       (%cleanup-sift3d ,sift3d))))

;; ===============
;; Keypoint store
;; ===============

(cffi:defcstruct (keypoint-store :size 48))

(cffi:defcfun ("init_Keypoint_store" %init-keypoint-store) :int
  (store (:pointer (:struct keypoint-store))))

(cffi:defcfun ("cleanup_Keypoint_store" %cleanup-keypoint-store) :void
  (store (:pointer (:struct keypoint-store))))

(defun init-keypoint-store (store)
  (unless (zerop (%init-keypoint-store store))
    (error 'util:ffi-error :message "Cannot allocate keypoint store")))

(defmacro with-keypoint-store ((store) &body body)
  `(cffi:with-foreign-object (,store '(:struct keypoint-store))
     (init-keypoint-store ,store)
     (unwind-protect
          (progn
            ,@body)
       (%cleanup-keypoint-store ,store))))

;; ================
;; Descriptor store
;; ================

(cffi:defcstruct (descriptor-store :size 32))

(cffi:defcfun ("init_SIFT3D_Descriptor_store" %init-descriptor-store) :int
  (store (:pointer (:struct descriptor-store))))

(cffi:defcfun ("cleanup_SIFT3D_Descriptor_store" %cleanup-descriptor-store) :void
  (store (:pointer (:struct descriptor-store))))

(defun init-descriptor-store (store)
  (unless (zerop (%init-descriptor-store store))
    (error 'util:ffi-error :message "Cannot allocate descriptor store")))

(defmacro with-descriptor-store ((store) &body body)
  `(cffi:with-foreign-object (,store '(:struct descriptor-store))
     (init-descriptor-store ,store)
     (unwind-protect
          (progn
            ,@body)
       (%cleanup-descriptor-store ,store))))

;; =========
;; Matrices
;; =========

;; But first of all the datatypes
(defconstant +type-double+ 0)
(defconstant +type-float+  1)
(defconstant +type-int+    2)

(cffi:defcstruct matrix
  (data   (:pointer :float))
  (size   :uint64) ;; FIXME: size_t!
  (ncols  :int)
  (nrows  :int)
  (unused :int)
  (type   :int))

(cffi:defcfun ("init_Mat_rm" %init-matrix) :int
  (matrix   (:pointer (:struct matrix)))
  (nrows    :int)
  (ncols    :int)
  (type     :int) ;; This is a fucking enum!
  (set-zero :int))

(cffi:defcfun ("cleanup_Mat_rm" %cleanup-matrix) :void
  (matrix (:pointer (:struct matrix))))

(defun init-matrix (matrix nrows ncols type set-zero-p)
  (unless (zerop (%init-matrix matrix nrows ncols type
                               (if set-zero-p 1 0)))
    (error 'util:ffi-error :message "Cannot initialize a matrix")))

(defmacro with-matrix ((matrix) &body body)
  `(cffi:with-foreign-object (,matrix '(:struct matrix))
     (init-matrix ,matrix 0 0 +type-float+ nil)
     (unwind-protect
          (progn
            ,@body)
       (%cleanup-matrix ,matrix))))

;; ==============================================
;; Keypoint detection and descriptors extraction
;; ==============================================

(cffi:defcfun ("set_peak_thresh_SIFT3D" %set-peak-threshold) :int
  (sift3d    (:pointer (:struct sift3d)))
  (threshold :double))

(defun set-peak-threshold (sift3d threshold)
  (unless (zerop (%set-peak-threshold sift3d threshold))
    (error 'util:ffi-error :message "Cannot set peak threshold")))

(cffi:defcfun ("SIFT3D_detect_keypoints" %detect-keypoints) :int
  (sift3d (:pointer (:struct sift3d)))
  (image  (:pointer (:struct image)))
  (store  (:pointer (:struct keypoint-store))))

(defun detect-keypoints (sift3d image store)
  (unless (zerop (%detect-keypoints sift3d image store))
    (error 'util:ffi-error :message "Cannot detect keypoints"))
  store)

(cffi:defcfun ("SIFT3D_extract_descriptors" %extract-descriptors) :int
  (sift3d           (:pointer (:struct sift3d)))
  (keypoint-store   (:pointer (:struct keypoint-store)))
  (descriptor-store (:pointer (:struct descriptor-store))))

(defun extract-descriptors (sift3d keypoint-store descriptor-store)
  (unless (zerop (%extract-descriptors sift3d keypoint-store descriptor-store))
    (error 'util:ffi-error :message "Cannot extract descriptors"))
  descriptor-store)

(cffi:defcfun ("SIFT3D_Descriptor_store_to_Mat_rm" %descriptors->matrix) :int
  (store  (:pointer (:struct descriptor-store)))
  (matrix (:pointer (:struct matrix))))

(defun descriptors->matrix (store matrix)
  (unless (zerop (%descriptors->matrix store matrix))
    (error 'util:ffi-error :message "Cannot copy descriptors to a matrix")))

;; =========================
;; The ultimate WITH- macro
;; =========================

;; Each binding is in the form (MACRO-NAME VAR &REST ARGS) where
;; MACRO-NAME is a part of the name after WITH-.
(defmacro with-sift3d-objects (bindings &body body)
  (car
   (reduce
    (lambda (binding acc)
      (destructuring-bind (macro-name var &rest args)
          binding
        (let ((macro-sym (intern (format nil "WITH-~a" macro-name))))
          `((,macro-sym (,var ,@args) ,@acc)))))
    bindings
    :from-end t
    :initial-value body)))

;; Now, high-level function for descriptor arrays
(serapeum:-> descriptors ((util:image single-float) &optional (double-float 0d0 1d0))
             (values list &optional))
(defun descriptors (array &optional (peak-threshold 1d-1))
  "Take an image (3D array of single-floats) and return a list of
descriptors. The parameter @c(PEAK-THRESHOLD) controls a number of
descriptors, with smaller value providing more descriptors. Providing
a value lesser than the default results in a great number of unstable
descriptors."
  (with-sift3d-objects ((sift3d           sift3d)
                        ;; Here we call TRANSPOSE because Sift3D
                        ;; library accepts arrays in column-major
                        ;; order (but outputs normal row-major ordered
                        ;; arrays).
                        (image            image (util:transpose array))
                        (keypoint-store   keypoint-store)
                        (descriptor-store descriptor-store)
                        (matrix           matrix))
    (set-peak-threshold sift3d peak-threshold)
    (detect-keypoints sift3d image keypoint-store)
    (extract-descriptors sift3d keypoint-store descriptor-store)
    (descriptors->matrix descriptor-store matrix)
    (let ((n (cffi:foreign-slot-value matrix '(:struct matrix) 'nrows))
          (desc-length (cffi:foreign-slot-value matrix '(:struct matrix) 'ncols))
          (matrix-data (cffi:foreign-slot-value matrix '(:struct matrix) 'data)))
      (unless (= desc-length 771)
        (error 'util:ffi-error :message "Got strange descriptors"))
      (loop for i below n
            for idx = (* i desc-length)
            collect
            ;; A descriptor is a vector of 771 single float elements.
            ;; The first 3 elements are the keypoint's coordinate and
            ;; the rest are arbitrary numbers which form a metric
            ;; space [0, 1]^{768} with a Euclidean metric.
            (let ((descriptor (make-array desc-length :element-type 'single-float)))
              (loop for j below desc-length do
                    (setf (aref descriptor j)
                          (cffi:mem-aref matrix-data :float (+ idx j))))
              descriptor)))))
