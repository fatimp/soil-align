(defpackage soil-align/match
  (:use #:cl)
  (:local-nicknames (#:util #:soil-align/util)
                    (#:ff   #:float-features))
  (:export #:match-descriptors))
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

(cffi:defcfun ("knn_search" %knn-search) :void
  (qs      (:pointer :float))
  (ps      (:pointer :float))
  (d       :size)
  (nqs     :size)
  (nps     :size)
  (dists   (:pointer :float))
  (indices (:pointer :uint64))
  (k       :size))

(deftype pair-array (type) `(simple-array ,type (* 2)))
(deftype dist-array    () '(pair-array single-float))
(deftype indices-array () '(pair-array (unsigned-byte 64)))

(serapeum:-> find-closest ((util:fixed-entries *)
                           (util:fixed-entries *))
             (values dist-array indices-array &optional))
(defun find-closest (s1 s2)
  (assert (= (array-dimension s1 1)
             (array-dimension s2 1)))
  (let* ((nqs (array-dimension s1 0))
         (dists   (make-array (list nqs 2) :element-type 'single-float))
         (indices (make-array (list nqs 2) :element-type '(unsigned-byte 64))))
  (cffi:with-pointer-to-vector-data (s1-ptr (sb-ext:array-storage-vector s1))
    (cffi:with-pointer-to-vector-data (s2-ptr (sb-ext:array-storage-vector s2))
      (cffi:with-pointer-to-vector-data (dists-ptr (sb-ext:array-storage-vector dists))
        (cffi:with-pointer-to-vector-data (indices-ptr (sb-ext:array-storage-vector indices))
          (%knn-search s1-ptr s2-ptr (array-dimension s1 1) nqs (array-dimension s2 0)
                       dists-ptr indices-ptr 2)))))
    (values dists indices)))

(serapeum:-> match-descriptors ((util:fixed-entries #.util:+descriptor-offset+)
                                (util:fixed-entries #.util:+descriptor-offset+)
                                (util:fixed-entries *)
                                (util:fixed-entries *)
                                &optional (single-float 0.0))
             (values list &optional))
(defun match-descriptors (c1 c2 d1 d2 &optional (c 1.2))
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
    ;; FIND-CLOSEST returns squared distances
    (let (matches (c (expt c 2)))
      (multiple-value-bind (dists indices)
          (find-closest d1 d2)
        (loop for i below (array-dimension dists 0)
              for d1 = (aref dists i 0)
              for d2 = (aref dists i 1)
              when (< (* d1 c) d2) do
              (push (cons (row c1 i)
                          (row c2 (aref indices i 0)))
                    matches)))
      matches)))
