(defclass c->so (asdf:source-file)
  ()
  (:default-initargs
   :type "c"))

(defun dynamic-library-extension ()
  "Return the dynamic library extension on the current OS as a string."
  (cond
    ((uiop:os-windows-p) "dll")
    ((uiop:os-macosx-p)  "dylib")
    ((uiop:os-unix-p)    "so")
    (t                   (error "unsupported OS"))))

(defmethod output-files ((operation compile-op) (component c->so))
  (values (list (asdf:apply-output-translations
                 (make-pathname :name (pathname-name (component-pathname component))
                                :type (dynamic-library-extension)
                                :defaults (component-pathname component))))
          t))

(defmethod perform ((operation load-op) (component c->so))
  t)

(defmethod perform ((operation compile-op) (component c->so))
  (flet ((nn (x) (uiop:native-namestring x)))
    (let* ((c-file (component-pathname component))
           (shared-object (first (output-files operation component))))
      (ensure-directories-exist shared-object)
      ;; TODO: Discover all needed flags
      (uiop:run-program
       (list "cc" "-O2" "-fPIC" "-shared"
             "-I/usr/local/include/python3.11"
             "-L/usr/local/lib"
             "-I/usr/local/lib/python3.11/site-packages/numpy/core/include"
             "-o" (nn shared-object) (nn c-file)
             "-lpython3.11")))))

(defsystem soil-align/pynndescent-so
  :name :soil-align/pynndescent-so
  :pathname "src"
  :components ((:c->so "libpynndescent-wrapper")))

(defsystem :soil-align
  :name :soil-align
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "Align images of soil"
  :pathname "src"
  :serial t
  :class :package-inferred-system
  :depends-on (:serapeum
               :alexandria
               :float-features
               :cffi
               :magicl
               :log4cl
               :parse-float
               :command-line-parse
               :numpy-npy
               :soil-align/pynndescent-so
               :soil-align/util
               :soil-align/preprocessing
               :soil-align/sift3d
               :soil-align/matches-bruteforce
               :soil-align/matches-pynndescent
               :soil-align/transform
               :soil-align/array-transform
               :soil-align/cli)
  :build-operation program-op
  :build-pathname "soil-align"
  :entry-point "soil-align/cli:main")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c)
                   :executable t
                   :compression -1))
