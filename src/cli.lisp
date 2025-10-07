(defpackage soil-align/cli
  (:use #:cl #:command-line-parse #:parse-float)
  (:local-nicknames (#:nndescent #:soil-align/matches-pynndescent)
                    (#:brute     #:soil-align/matches-bruteforce)
                    (#:util      #:soil-align/util)
                    (#:sift3d    #:soil-align/sift3d)
                    (#:db        #:soil-align/db)
                    (#:pre       #:soil-align/preprocessing)
                    (#:trans     #:soil-align/transform)
                    (#:atrans    #:soil-align/array-transform))
  (:export #:main))
(in-package :soil-align/cli)

(alexandria:define-constant +db-pathname+
    #+unix
    #p"~/.local/share/soil-align/descriptors.sqlite"
    #-unix
    (error "I don't know a suitable location where I can store the database.")
  :documentation "Path where the cache is stored"
  :test #'equalp)

(alexandria:define-constant +log-pathname+
    #+unix
    #p"~/.local/share/soil-align/log"
    #-unix
    (error "I don't know a suitable location where I can store the log file.")
  :documentation "Path where the log is stored"
  :test #'equalp)

(serapeum:-> get-db-pathname ()
             (values pathname &optional))
(defun get-db-pathname ()
  (let ((override (sb-posix:getenv "SOIL_ALIGN_DB")))
    (if override (pathname override) +db-pathname+)))

(defun parse-ratio-<1 (string)
  (let ((x (parse-float string)))
    (unless (<= 0 x 1)
      (error "A ratio must be in the range [0, 1]"))
    x))

(defun parse-ratio->1 (string)
  (let ((x (parse-float string)))
    (unless (>= x 1)
      (error "A ratio must be bigger than 1"))
    x))

(defparameter *parser*
  (seq
   (optional
    (flag   :verbose
            :short       #\v
            :long        "verbose"
            :description "Be verbose")
    (flag   :bruteforce
            :long        "bruteforce"
            :description "Use bruteforce matching of keypoints instead of pynndescent")
    (flag   :no-db
            :long        "no-db"
            :description "Do not cache image descriptors in the database")
    (option :nthreads    "N"
            :long        "threads"
            :short       #\t
            :fn          #'parse-integer
            :description "Number of threads to use")
    (option :transform-matrix "m.npy"
            :short       #\m
            :long        "matrix"
            :description "Output file name for a transform matrix (in numpy format)")
    (option :transformed-image "out.npy"
            :short       #\o
            :long        "image"
            :description "Output file name for a transformed image (in numpy format)")
    (option :min-dog     "P"
            :long        "min-dog"
            :description "The smallest allowed absolute DoG value, as a fraction of the largest"
            :fn          #'parse-ratio-<1)
    (option :dist-ratio  "C"
            :long        "dist-ratio"
            :description "Controls what we consider a match"
            :fn          #'parse-ratio->1)
    (option :fit-error   "E"
            :long        "fit-error"
            :description "The maximal allowed fit error to treat a sample as inlier"
            :fn          #'parse-float)
    (option :min-inliers "N"
            :long        "min-inliers"
            :description "A fraction of inliers to accept a fit [0-1]"
            :fn          #'parse-ratio-<1))
   (argument :reference "reference")
   (argument :source    "source")))

(defun print-usage-and-quit ()
  (print-usage *parser* "soil-align")
  (uiop:quit 1))

(defun get-arguments-or-fail ()
  (handler-case
      (parse-argv *parser*)
    (error () (print-usage-and-quit))))

(defmacro with-pynndescent (initialize &body body)
  `(cond
     (,initialize
      (nndescent:nndescent-initialize)
      (unwind-protect
           (progn ,@body)
        (nndescent:nndescent-deinitialize)))
     (t
      ,@body)))

(declaim (inline find-matches))
(defun find-matches (s1 s2 bruteforcep c)
  (if bruteforcep
      (brute:match-descriptors s1 s2 c)
      (nndescent:match-descriptors s1 s2 c)))

;; Fucking LOG:INFO is a macro too
(defmacro log-eval (computation &rest args)
  `(prog1 ,computation
     (log:info ,@args)))

(declaim (inline))
(serapeum:-> descriptors
             ((util:image (unsigned-byte 8))
              (double-float 0d0 1d0)
              (or pathname null))
             (values list &optional))
(defun descriptors (array peak-threshold db-pathname)
  (if db-pathname
      (db:descriptors-cached array #'pre:clahe db-pathname peak-threshold)
      (sift3d:descriptors (pre:clahe array) peak-threshold)))

(declaim (inline default-thread-number))
(defun default-thread-number ()
  #+freebsd
  (min (floor (freebsd-sysctl:sysctl-by-name "kern.smp.cores") 2) 10)
  #-freebsd
  (progn
    (log:warn
     #.(concatenate
        'string
        "Cannot get default number of threads and will use only 1. "
        "Use --threads to override this behavior."))
    1))

(defun main ()
  (sb-ext:disable-debugger)
  (let* ((args (get-arguments-or-fail))
         (min-dog (float (%assoc :min-dog           args 0.1) 0d0))
         (bruteforcep    (%assoc :bruteforce        args))
         (min-inliers    (%assoc :min-inliers       args 0.8))
         (dist-ratio     (%assoc :dist-ratio        args 1.3))
         (fit-error      (%assoc :fit-error         args 100.0))
         (trans-image    (%assoc :transformed-image args))
         (trans-matrix   (%assoc :transform-matrix  args))
         (reference      (%assoc :reference         args))
         (source         (%assoc :source            args))
         (no-db-p        (%assoc :no-db             args))
         ;; Do not evaluate default here
         (nthreads       (%assoc :nthreads          args))
         (db-pathname (if (not no-db-p) (get-db-pathname))))
    (unless (or trans-image trans-matrix)
      (format *error-output* "No output selected~%")
      (print-usage-and-quit))
    (log:config (if (%assoc :verbose args) :info :warn))
    (log:config :daily +log-pathname+ :backup nil)
    (let ((source    (numpy-npy:load-array source))
          (reference (numpy-npy:load-array reference)))
      (unless (and (equalp (array-element-type source)    '(unsigned-byte 8))
                   (equalp (array-element-type reference) '(unsigned-byte 8)))
        (format *error-output* "Both input arrays must have dtype='uint8'")
        (print-usage-and-quit))
      (log:info "Starting")
      (let ((nthreads (or nthreads (default-thread-number))))
        (log:info "Will use ~d threads" nthreads)
        (sift3d:set-num-threads nthreads))
      (with-pynndescent (not bruteforcep)
        (let* ((desc-source
                (log-eval (descriptors source min-dog db-pathname)
                          "Got descriptors of the source image"))
               (desc-reference
                (log-eval (descriptors reference min-dog db-pathname)
                          "Got descriptors of the reference image"))
               (matches
                (log-eval
                 (find-matches desc-reference desc-source bruteforcep dist-ratio)
                 "Found matches between images")))
          (multiple-value-bind (matrix error inliers)
              (trans:affine-transform matches
                                      :min-inliers min-inliers
                                      :max-iter    2000
                                      :err         fit-error)
            (log:info "Found a transform matrix")
            (unless matrix
              (log:error "Consensus is not achieved")
              (uiop:quit 0))
            (when trans-matrix
              (numpy-npy:store-array matrix trans-matrix))
            (when trans-image
              (numpy-npy:store-array
               (log-eval
                (atrans:apply-transform source matrix)
                "Computed a transformed image")
               trans-image))
            (log:info "Summary: ~d/~d descriptors, ~d matches, ~d inliers, ~f fit error"
                      (length desc-source)
                      (length desc-reference)
                      (length matches)
                      inliers error))))))
  (uiop:quit 0))
