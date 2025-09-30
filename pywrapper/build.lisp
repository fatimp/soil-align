(defpackage pywrapper
  (:use #:cl #:split-sequence))
(in-package :pywrapper)

(defclass asdf::pywrapper (asdf:source-file)
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

(defun get-python-flags ()
  (remove-if
   (lambda (token)
     (member token (list "" (concatenate 'string '(#\NewLine))) :test #'string=))
   (split-sequence
    #\Space
    (with-output-to-string (out)
      (uiop:run-program
       '("python-config" "--cflags" "--ldflags" "--embed")
       :output out)))))

(defun get-numpy-flags ()
  (concatenate
   'string "-I"
   (with-output-to-string (out)
     (uiop:run-program
      '("python" "-c" "import numpy; print (numpy.get_include (), end = '')")
      :output out))))

(defmethod asdf:output-files ((operation asdf:compile-op) (component asdf::pywrapper))
  (values (list (asdf:apply-output-translations
                 (make-pathname :name (pathname-name (asdf:component-pathname component))
                                :type (dynamic-library-extension)
                                :defaults (asdf:component-pathname component))))
          t))

(defmethod asdf:perform ((operation asdf:load-op) (component asdf::pywrapper))
  t)

(defmethod asdf:perform ((operation asdf:compile-op) (component asdf::pywrapper))
  (flet ((nn (x) (uiop:native-namestring x)))
    (let* ((c-file (asdf:component-pathname component))
           (shared-object (first (asdf:output-files operation component))))
      (ensure-directories-exist shared-object)
      ;; TODO: Discover all needed flags
      (uiop:run-program
       (list* "cc" "-fPIC" "-shared"
              "-I/usr/local/lib/python3.11/site-packages/numpy/core/include"
              "-o" (nn shared-object) (nn c-file)
              (get-numpy-flags)
              (get-python-flags))))))
