(defsystem :soil-align
  :name :soil-align
  :version "0.10.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "Align images of soil"
  :pathname "src"
  :serial t
  :class :package-inferred-system
  :depends-on (:serapeum
               :alexandria
               :float-features
               :cffi
               :entzauberte-matrices
               :log4cl
               :parse-float
               :command-line-parse
               :numpy-npy
               :nibbles
               :ironclad
               :lmdb
               :vector-sum
               :lparallel
               :cl-conspack
               :cl-libtiff
               (:feature :freebsd :freebsd-sysctl)
               :soil-align/util
               :soil-align/preprocessing
               :soil-align/pca
               :soil-align/sift3d
               :soil-align/db
               :soil-align/match
               :soil-align/transform
               :soil-align/array-transform
               :soil-align/io
               :soil-align/cli)
  :in-order-to ((test-op (load-op "soil-align/tests")))
  :perform (test-op (op system)
                    (declare (ignore op system))
                    (funcall
                     (symbol-function
                      (intern (symbol-name '#:run-tests)
                              (find-package :soil-align/tests)))))
  :build-operation program-op
  :build-pathname "soil-align"
  :entry-point "soil-align/cli:main")

(defsystem :soil-align/tests
  :name :soil-align/tests
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :pathname "tests"
  :components ((:file "package")
               (:file "tests" :depends-on ("package")))
  :depends-on (:soil-align :fiveam :approx))

;; For qlot
(defsystem :soil-align/docs
    :depends-on (:soil-align :codex))

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c)
                   :executable t
                   :compression -1))
