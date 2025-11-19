(defsystem soil-align/pynndescent-so
  :name :soil-align/pynndescent-so
  :defsystem-depends-on (:pywrapper)
  :pathname "src"
  :components ((:pywrapper "libpynndescent-wrapper")))

(defsystem :soil-align
  :name :soil-align
  :version "0.5"
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
               :nibbles
               :ironclad
               :vector-sum
               :cl-conspack
               :climp
               (:feature :freebsd :freebsd-sysctl)
               :soil-align/pynndescent-so
               :soil-align/util
               :soil-align/preprocessing
               :soil-align/pca
               :soil-align/sift3d
               :soil-align/cache
               :soil-align/match
               :soil-align/transform
               :soil-align/array-transform
               :soil-align/io
               :soil-align/cli)
  :build-operation program-op
  :build-pathname "soil-align"
  :entry-point "soil-align/cli:main")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c)
                   :executable t
                   :compression -1))
