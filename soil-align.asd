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
               :soil-align/util
               :soil-align/preprocessing
               :soil-align/sift3d
               :soil-align/matches
               :soil-align/transform
               :soil-align/array-transform))
