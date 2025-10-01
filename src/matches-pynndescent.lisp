;; Dong, Wei, Charikar Moses, and Kai Li. "Efficient k-nearest
;; neighbor graph construction for generic similarity measures."
;; Proceedings of the 20th international conference on World wide
;; web. ACM, 2011.
;;
;; Though implementing that algorithm exactly gives very poor results,
;; that's why we need this python library.

(defpackage soil-align/matches-pynndescent
  (:use #:cl)
  (:local-nicknames (#:util #:soil-align/util)
                    (#:ff   #:float-features))
  (:export #:match-descriptors
           #:nndescent-initialize
           #:nndescent-deinitialize))
(in-package :soil-align/matches-pynndescent)

(let ((pathname
       (asdf:output-file
        'asdf:compile-op
        (asdf:find-component
         :soil-align/pynndescent-so "libpynndescent-wrapper"))))
  (pushnew
   (make-pathname
    :directory (pathname-directory pathname))
   cffi:*foreign-library-directories*
   :test #'equalp))

(cffi:define-foreign-library libpynndescent-wrapper
  (:unix  (:or "libpynndescent-wrapper.so"))
  (t (:default "libpynndescent-wrapper")))
(cffi:use-foreign-library libpynndescent-wrapper)

(cffi:defcfun ("nndescent_find_closest" %find-closest) :void
  (set1 (:pointer :float))
  (len1 :uint64) ; FIXME: size_t
  (set2 (:pointer :float))
  (len2 :uint64) ; FIXME: size_t
  (callback :pointer))

(cffi:defcfun ("nndescent_initialize" %nndescent-initialize) :bool)
(cffi:defcfun ("nndescent_deinitialize" nndescent-deinitialize) :void)

(defun nndescent-initialize ()
  (ff:with-float-traps-masked (:divide-by-zero :invalid :overflow)
    (unless (%nndescent-initialize)
      (error 'util:ffi-error :message "Cannot initialize pynndescent"))))

(defvar *indices*)
(defvar *dists*)

(cffi:defcallback result-callback :void
    ((dists-ptr   (:pointer :float))
     (indices-ptr (:pointer :float))
     (len         :uint64)) ; size_t
  (let ((dists   (make-array (list len 2) :element-type 'single-float))
        (indices (make-array (list len 2) :element-type '(unsigned-byte 32))))
    (loop for i below (* len 2) do
          (setf (row-major-aref dists i)
                (cffi:mem-aref dists-ptr :float i)
                (row-major-aref indices i)
                (cffi:mem-aref indices-ptr :int32 i)))
    (setq *indices* indices
          *dists* dists)))

(deftype descriptor-array () '(simple-array single-float (* #.util:+descriptor-length+)))
(deftype pair-array (type) `(simple-array ,type (* 2)))
(deftype dist-array    () '(pair-array single-float))
(deftype indices-array () '(pair-array (unsigned-byte 32)))

(serapeum:-> find-closest (descriptor-array descriptor-array)
             (values dist-array indices-array &optional))
(defun find-closest (s1 s2)
  (let (*indices* *dists*)
    (cffi:with-pointer-to-vector-data (s1-ptr (sb-ext:array-storage-vector s1))
      (cffi:with-pointer-to-vector-data (s2-ptr (sb-ext:array-storage-vector s2))
        (%find-closest s1-ptr (array-dimension s1 0)
                       s2-ptr (array-dimension s2 0)
                       (cffi:callback result-callback))))
    (unless (and *dists* *indices*)
      (error 'util:ffi-error :message "Cannot find nearest neighbors"))
    (values *dists* *indices*)))

(serapeum:-> match-descriptors (list list &optional (single-float 0.0))
             (values list &optional))
(defun match-descriptors (s1 s2 &optional (c 1.3))
  "Find matches between two sets of descriptors. The parameter @c(C)
controls what we treat as a match. Bigger values result in a lesser
number of more stable matches.

Pynndescent is required for this function."
  (flet ((cut-coordinates (descriptor)
           (declare (type util:descriptor descriptor))
           (subseq descriptor util:+descriptor-offset+))
         (cut-descriptor (descriptor)
           (declare (type util:descriptor descriptor))
           (subseq descriptor 0 util:+descriptor-offset+))
         (row (xs i)
           (let ((dim (array-dimension xs 1)))
             (make-array dim
                         :element-type 'single-float
                         :initial-contents (loop for j below dim collect
                                                 (aref xs i j))))))
    (let ((s1 (make-array (list (length s1) util:+descriptor-length+)
                          :element-type 'single-float
                          :initial-contents (mapcar #'cut-coordinates s1)))
          (s2 (make-array (list (length s2) util:+descriptor-length+)
                          :element-type 'single-float
                          :initial-contents (mapcar #'cut-coordinates s2)))
          (c1 (make-array (list (length s1) 3)
                          :element-type 'single-float
                          :initial-contents (mapcar #'cut-descriptor s1)))
          (c2 (make-array (list (length s2) 3)
                          :element-type 'single-float
                          :initial-contents (mapcar #'cut-descriptor s2)))
          matches)
      (multiple-value-bind (dists indices)
          (find-closest s1 s2)
        (loop for i below (array-dimension dists 0)
              for d1 = (aref dists i 0)
              for d2 = (aref dists i 1)
              when (< (* d1 c) d2) do
              (push (cons (row c1 i)
                          (row c2 (aref indices i 0)))
                    matches)))
      matches)))
