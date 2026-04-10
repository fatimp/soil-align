(defpackage soil-align/match
  (:use #:cl)
  (:local-nicknames (#:util #:soil-align/util)
                    (#:ff   #:float-features))
  (:export #:match-descriptors
           #:nn-initialize
           #:nn-deinitialize
           #:with-nn))
(in-package :soil-align/match)

(let ((pathname
       (asdf:output-file
        'asdf:compile-op
        (asdf:find-component
         :soil-align/libnn-so "libnn"))))
  (pushnew
   (make-pathname
    :directory (pathname-directory pathname))
   cffi:*foreign-library-directories*
   :test #'equalp))

(cffi:define-foreign-library libnn-wrapper
  (:unix  (:or "libnn.so"))
  (t (:default "libnn")))
(cffi:use-foreign-library libnn-wrapper)

(cffi:defcfun ("nn_find_closest" %find-closest) :void
  (set1      (:pointer :float))
  (len1      :uint64) ; FIXME: size_t
  (set2      (:pointer :float))
  (len2      :uint64) ; FIXME: size_t
  (nfeatures :uint64) ; FIXME: size_t
  (njobs     :int)
  (callback  :pointer))

(cffi:defcfun ("nn_initialize" %nn-initialize) :bool)
(cffi:defcfun ("nn_deinitialize" nn-deinitialize) :void)

(defun nn-initialize ()
  (ff:with-float-traps-masked (:divide-by-zero :invalid :overflow)
    (unless (%nn-initialize)
      (error 'util:ffi-error :message "Cannot initialize scikit"))))

(defmacro with-nn (&body body)
  "Initialize python for nearest neighbors queries and execute
@c(body). When control leaves body python is safely deinitialized."
  `(progn
     (nn-initialize)
     (unwind-protect
          (progn ,@body)
       (nn-deinitialize))))

(defvar *indices*)
(defvar *dists*)

(cffi:defcallback result-callback :void
    ((dists-ptr   (:pointer :float))
     (indices-ptr (:pointer :int32))
     (len         :uint64)) ; size_t
  (let ((dists   (make-array (list len 2) :element-type 'single-float))
        (indices (make-array (list len 2) :element-type '(unsigned-byte 64))))
    (loop for i below (* len 2) do
          (setf (row-major-aref dists i)
                (cffi:mem-aref dists-ptr :float i)
                (row-major-aref indices i)
                (cffi:mem-aref indices-ptr :uint64 i)))
    (setq *indices* indices
          *dists* dists)))

(deftype pair-array (type) `(simple-array ,type (* 2)))
(deftype dist-array    () '(pair-array single-float))
(deftype indices-array () '(pair-array (unsigned-byte 64)))

(serapeum:-> find-closest ((util:fixed-entries *)
                           (util:fixed-entries *)
                           (integer 1))
             (values dist-array indices-array &optional))
(defun find-closest (s1 s2 njobs)
  (assert (= (array-dimension s1 1)
             (array-dimension s2 1)))
  (let (*indices* *dists*)
    (cffi:with-pointer-to-vector-data (s1-ptr (sb-ext:array-storage-vector s1))
      (cffi:with-pointer-to-vector-data (s2-ptr (sb-ext:array-storage-vector s2))
        (%find-closest s1-ptr (array-dimension s1 0)
                       s2-ptr (array-dimension s2 0)
                       (array-dimension s1 1) njobs
                       (cffi:callback result-callback))))
    (unless (and *dists* *indices*)
      (error 'util:ffi-error :message "Cannot find nearest neighbors"))
    (values *dists* *indices*)))

(serapeum:-> match-descriptors ((util:fixed-entries #.util:+descriptor-offset+)
                                (util:fixed-entries #.util:+descriptor-offset+)
                                (util:fixed-entries *)
                                (util:fixed-entries *) &key
                                (:c (single-float 0.0))
                                (:njobs (integer 1)))
             (values list &optional))
(defun match-descriptors (c1 c2 d1 d2 &key (njobs 1) (c 1.2))
  "Find matches between two sets of descriptors. The parameter @c(C)
controls what we treat as a match. Bigger values result in a lesser
number of more stable matches. \\(C_i\\) is an array of keypoint
coordinates and \\(D_i\\) is an array of corresponding descriptors."
  (declare (optimize (speed 3)))
  (flet ((row (xs i)
           (let ((dim (array-dimension xs 1)))
             (make-array dim
                         :element-type 'single-float
                         :initial-contents (loop for j below dim collect
                                                 (aref xs i j))))))
    (let (matches)
      (multiple-value-bind (dists indices)
          (find-closest d1 d2 njobs)
        (loop for i below (array-dimension dists 0)
              for d1 = (aref dists i 0)
              for d2 = (aref dists i 1)
              when (< (* d1 c) d2) do
              (push (cons (row c1 i)
                          (row c2 (aref indices i 0)))
                    matches)))
      matches)))
