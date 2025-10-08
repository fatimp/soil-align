(defpackage soil-align/io
  (:use #:cl)
  (:local-nicknames (#:util #:soil-align/util))
  (:export #:read-image
           #:write-image))
(in-package :soil-align/io)

;; Raw "format" (a big mispractice of our NII)
(serapeum:-> read-raw (pathname)
             (values (util:image (unsigned-byte 8)) &optional))
(defun read-raw (pathname)
  (let ((specification
         (with-open-file (input pathname)
           (uiop:with-safe-io-syntax (:package :cl)
             (read input nil)))))
    (unless (and (null (cddddr specification))
                 (cdddr specification))
      (error 'util:io-error
             :message "Raw format spec must be (FILENAME DIM-X DIM-Y DIM-Z)"))
    (let ((raw-pathname (merge-pathnames
                         (pathname (first specification))
                         pathname))
          (result (make-array (cdr specification)
                              :element-type '(unsigned-byte 8))))
      (with-open-file (input raw-pathname :element-type '(unsigned-byte 8))
        (unless (= (read-sequence (sb-ext:array-storage-vector result) input)
                   (array-total-size result))
          (error 'util:io-error
                 :message "The input file is too short")))
      result)))

(serapeum:-> write-raw ((util:image (unsigned-byte 8)) pathname)
             (values &optional))
(defun write-raw (array pathname)
  (with-open-file (output pathname
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede
                          :element-type '(unsigned-byte 8))
    (write-sequence (sb-ext:array-storage-vector array) output))
  (values))

;; General I/O facility

(declaim (inline check-element-type))
(defun check-element-type (array)
  (unless (equalp (array-element-type array) '(unsigned-byte 8))
    (error 'util:io-error
           :message "Input array must have dtype='uint8'"))
  array)

;; TODO: detection based on content rather on name
(serapeum:-> read-image
             ((or string pathname))
             (values (util:image (unsigned-byte 8)) &optional))
(defun read-image (pathname)
  (let* ((pathname (pathname pathname))
         (type (pathname-type pathname)))
    (check-element-type
     (cond
       ((string= type "npy")
        (numpy-npy:load-array pathname))
       ((string= type "s-exp")
        (read-raw pathname))
       (t (error 'util:io-error
                 :message "Unsupported input format"))))))

(serapeum:-> write-image
             ((util:image (unsigned-byte 8)) (or string pathname))
             (values &optional))
(defun write-image (array pathname)
  (let* ((pathname (pathname pathname))
         (type (pathname-type pathname)))
    (cond
      ((string= type "npy")
       (numpy-npy:store-array array pathname))
      ((string= type "raw")
       (write-raw array pathname))
      (t (error 'util:io-error
                 :message "Unsupported output format")))))
