(defsystem :pywrapper
  :name :pywrapper
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "Building facility for soil-align"
  :components ((:file "build"))
  :depends-on (:cl-ppcre))
