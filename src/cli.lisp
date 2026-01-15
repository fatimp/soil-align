(defpackage soil-align/cli
  (:use #:cl #:command-line-parse #:parse-float)
  (:local-nicknames (#:match  #:soil-align/match)
                    (#:util   #:soil-align/util)
                    (#:sift3d #:soil-align/sift3d)
                    (#:db     #:soil-align/db)
                    (#:io     #:soil-align/io)
                    (#:pca    #:soil-align/pca)
                    (#:trans  #:soil-align/transform)
                    (#:atrans #:soil-align/array-transform)
                    (#:em     #:entzauberte-matrices))
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

(defun parse-threshold (string)
  (let ((x (parse-float string :type 'double-float)))
    (unless (<= 0 x 1)
      (error 'util:user-input-error
             :message "The peak threshold must be in the range [0, 1]"))
    x))

(defun parse-dist-ratio (string)
  (let ((x (parse-float string)))
    (unless (>= x 1)
      (error 'util:user-input-error :message "The distance ratio must be bigger than 1"))
    x))

(defparameter *parser*
  (seq
   (optional
    (flag   :verbose
            :short       #\v
            :long        "verbose"
            :description "Be verbose")
    (option :nthreads    "N"
            :long        "threads"
            :short       #\t
            :fn          #'parse-integer
            :description "Number of threads to use")
    (option :workspace-side "S"
            :long        "workspace-side"
            :short       #\w
            :fn          #'parse-integer
            :description "Side of a workspace which is cut from center of the input images")
    (option :transform-matrix "m.npy"
            :short       #\m
            :long        "matrix"
            :description "Output file name for a transform matrix (.npy)")
    (option :transformed-image "out.npy"
            :short       #\o
            :long        "image"
            :description "Output file name for a transformed image (.npy or .raw)")
    (option :min-dog     "P"
            :long        "min-dog"
            :description "The smallest allowed absolute DoG value, as a fraction of the largest"
            :fn          #'parse-threshold)
    (option :dist-ratio  "C"
            :long        "dist-ratio"
            :description "Controls what we consider a match"
            :fn          #'parse-dist-ratio)
    (option :fit-error   "E"
            :long        "fit-error"
            :description "The maximal allowed fit error to treat a sample as inlier"
            :fn          #'parse-float)
    (option :ransac-iter "M"
            :long        "ransac-iterations"
            :description "Number of RANSAC iterations"
            :fn          #'parse-integer))
   (argument :reference "reference")
   (argument :source    "source")))

(defmacro with-pynndescent (&body body)
  `(progn
     (match:nndescent-initialize)
     (unwind-protect
          (progn ,@body)
       (match:nndescent-deinitialize))))

;; Fucking LOG:INFO is a macro too
(defmacro log-eval ((computation &optional (values 1)) &rest args)
  (let ((variables (loop repeat values collect (gensym))))
    `(multiple-value-bind ,variables ,computation
       (log:info ,@args)
       (values ,@variables))))

(declaim (inline number-of-threads))
(defun number-of-threads (n)
  (if n n
      #+freebsd
      (min (floor (freebsd-sysctl:sysctl-by-name "kern.smp.cores") 2) 10)
      #-freebsd
      (progn
        (log:warn
         #.(concatenate
            'string
            "Cannot get default number of threads and will use only 1. "
            "Use --threads to override this behavior."))
        1)))

(serapeum:-> load-and-maybe-cut
             ((or string pathname) (or null alexandria:positive-fixnum))
             (values (util:image (unsigned-byte 8))
                     alexandria:non-negative-fixnum
                     alexandria:non-negative-fixnum
                     alexandria:non-negative-fixnum
                     &optional))
(defun load-and-maybe-cut (pathname side)
  (let ((array (io:read-image pathname)))
    (if side
        (util:cut-from-center array side)
        (values array 0 0 0))))

(serapeum:-> add-offsets!
             (alexandria:non-negative-fixnum
              alexandria:non-negative-fixnum
              alexandria:non-negative-fixnum
              (util:fixed-entries #.util:+descriptor-offset+))
             (values (util:fixed-entries #.util:+descriptor-offset+) &optional))
(defun add-offsets! (off-x off-y off-z keypoints)
  (declare (optimize (speed 3)))
  (unless (= off-x off-y off-z 0)
    (loop for i below (array-dimension keypoints 0) do
          (incf (aref keypoints i 0) off-x)
          (incf (aref keypoints i 1) off-y)
          (incf (aref keypoints i 2) off-z)))
  keypoints)

(defun %main ()
  (let* ((args (parse-argv *parser*))
         (min-dog        (%assoc :min-dog           args 1d-1))
         (dist-ratio     (%assoc :dist-ratio        args 1.2))
         (fit-error      (%assoc :fit-error         args 100.0))
         (trans-image    (%assoc :transformed-image args))
         (trans-matrix   (%assoc :transform-matrix  args))
         (reference      (%assoc :reference         args))
         (source         (%assoc :source            args))
         (workspace-side (%assoc :workspace-side    args))
         (ransac-iter    (%assoc :ransac-iter       args 5000))
         (nthreads       (%assoc :nthreads          args))
         (nthreads (number-of-threads nthreads))
         (db-pathname (get-db-pathname)))
    (unless (or trans-image trans-matrix)
      (error 'util:user-input-error :message "No output selected"))
    (log:config (if (%assoc :verbose args) :info :warn))
    (log:config :daily +log-pathname+ :backup nil)
    (serapeum:mvlet ((source    sx sy sz (load-and-maybe-cut source    workspace-side))
                     (reference rx ry rz (load-and-maybe-cut reference workspace-side)))
      ;; Run a full GC because uncut arrays may be really big, we need
      ;; to collect them now because later we will run foreign code
      ;; which allocates a lot.
      (sb-ext:gc :full t)
      (log:info "Starting")
      (log:info "Will use ~d threads" nthreads)
      (em:set-num-threads nthreads)
      (with-pynndescent
        (serapeum:mvlet ((source-kp source-desc-pca source-vt source-means
                                    (log-eval
                                     ((db:descriptors-cached source db-pathname min-dog) 4)
                                     "Got descriptors of the source image"))
                         (ref-kp ref-desc-pca ref-vt ref-means
                                 (log-eval
                                  ((db:descriptors-cached reference db-pathname min-dog) 4)
                                  "Got descriptors of the reference image")))
          ;; Convert descriptors in ref PCA space
          (let* ((ref-desc ref-desc-pca)
                 (source-desc (pca:transform-pca
                               (pca:invert-pca source-desc-pca source-vt source-means)
                               ref-vt ref-means))
                 ;; Find matches between descriptors
                 (matches
                  (match:match-descriptors
                   (add-offsets! rx ry rz ref-kp)
                   (add-offsets! sx sy sz source-kp)
                   ref-desc source-desc dist-ratio)))
            (log:info "Found matches between images")
            (multiple-value-bind (matrix error inliers)
                (trans:rigid-transform matches
                                       :max-iter ransac-iter
                                       :err      fit-error)
              (unless matrix
                (log:info "Summary: ~d/~d descriptors, ~d matches"
                          (array-dimension source-kp 0)
                          (array-dimension ref-kp 0)
                          (length matches))
                (log:error "Consensus is not achieved")
                (uiop:quit 0))
              (log:info "Found a transform matrix")
              (when trans-matrix
                (numpy-npy:store-array matrix trans-matrix))
              (when trans-image
                (io:write-image
                 (log-eval
                  ((atrans:apply-transform
                    (if workspace-side
                        ;; Load a bigger image once more
                        (numpy-npy:load-array (%assoc :source args))
                        source)
                    matrix :nthreads nthreads))
                  "Computed a transformed image")
                 trans-image))
              (log:info #.(concatenate
                           'string
                           "Summary: ~d/~d descriptors, ~d independent parameters, "
                           "~d matches, ~d inliers, ~f fit error")
                        (array-dimension source-kp 0)
                        (array-dimension ref-kp 0)
                        (array-dimension ref-desc 1)
                        (length matches)
                        inliers error))))))))

(deftype foreign-user-input-error () '(or cmd-line-parse-error))

(defun handle-error (c)
  (princ c *error-output*)
  (terpri *error-output*)
  (if (typep c '(and (or util:internal-error (not util:generic-error))
                     (not foreign-user-input-error)))
      (sb-debug:backtrace 20 *error-output*)
      (print-usage *parser* "soil-align"))
  (uiop:quit 1))

(defun main ()
  (sb-ext:disable-debugger)
  (handler-bind
      ((error #'handle-error))
    (%main))
  (uiop:quit 0))
