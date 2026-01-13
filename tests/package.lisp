(defpackage soil-align/tests
  (:use #:cl #:fiveam #:approx)
  (:local-nicknames (#:util #:soil-align/util)
                    (#:pca  #:soil-align/pca)
                    (#:tran #:soil-align/transform))
  (:export #:run-tests))
