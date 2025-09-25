(defpackage soil-align/matches
  (:use #:cl)
  (:local-nicknames (#:util #:soil-align/util)
                    (#:ff   #:float-features))
  (:export #:match-descriptors/bruteforce))
(in-package :soil-align/matches)

(serapeum:-> descriptor-dist (util:descriptor util:descriptor)
             (values (single-float 0.0) &optional))
(defun descriptor-dist (d1 d2)
  (declare (optimize (speed 3)))
  (assert (= (length d1) (length d2)))
  (sqrt
   (loop for i from util:+descriptor-offset+ below (length d1) sum
         (expt (- (aref d1 i) (aref d2 i)) 2)
         single-float)))

(serapeum:-> find-2-nearest (util:descriptor list)
             (values util:descriptor util:descriptor
                     (single-float 0.0) (single-float 0.0)
                     &optional))
(defun find-2-nearest (desc set)
  (declare (optimize (speed 3)))
  (assert (cdr set))
  (let ((d1 ff:single-float-positive-infinity)
        (d2 ff:single-float-positive-infinity)
        c1 c2)
    (loop for %desc in set do
          (let ((dist (descriptor-dist %desc desc)))
            (cond
              ((< dist d1)
               (setq c2 c1
                     c1 %desc
                     d2 d1
                     d1 dist))
              ((< dist d2)
               (setq c2 %desc
                     d2 dist)))))
    (values c1 c2 d1 d2)))

(serapeum:-> match-descriptors/bruteforce
             (list list &optional (single-float 1.0))
             (values list &optional))
(defun match-descriptors/bruteforce (set1 set2 &optional (c 1.3))
  (declare (optimize (speed 3)))
  (let (matches)
    (loop for desc1 in set1 do
          (multiple-value-bind (desc2 %desc d1 d2)
              (find-2-nearest desc1 set2)
            (declare (ignore %desc))
            (when (> (/ d2 d1) c)
              (push (cons (subseq desc1 0 3)
                          (subseq desc2 0 3))
                    matches))))
    matches))
