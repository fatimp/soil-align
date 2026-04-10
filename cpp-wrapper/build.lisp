(defpackage cpp-wrapper
  (:use #:cl))
(in-package :cpp-wrapper)

(defclass asdf::cpp-wrapper (asdf:source-file)
  ()
  (:default-initargs
   :type "cc"))

(defun dynamic-library-extension ()
  "Return the dynamic library extension on the current OS as a string."
  (cond
    ((uiop:os-windows-p) "dll")
    ((uiop:os-macosx-p)  "dylib")
    ((uiop:os-unix-p)    "so")
    (t                   (error "unsupported OS"))))

(defmethod asdf:output-files ((operation asdf:compile-op) (component asdf::cpp-wrapper))
  (values (list (asdf:apply-output-translations
                 (make-pathname :name (pathname-name (asdf:component-pathname component))
                                :type (dynamic-library-extension)
                                :defaults (asdf:component-pathname component))))
          t))

(defmethod asdf:perform ((operation asdf:load-op) (component asdf::cpp-wrapper))
  t)

(defmethod asdf:perform ((operation asdf:compile-op) (component asdf::cpp-wrapper))
  (flet ((nn (x) (uiop:native-namestring x)))
    (let* ((c-file (asdf:component-pathname component))
           (shared-object (first (asdf:output-files operation component))))
      (ensure-directories-exist shared-object)
      (uiop:run-program
       (list "c++" "-fPIC" "-shared"
             #+freebsd "-I/usr/local/include"
             #+freebsd "-L/usr/local/lib"
             "-o" (nn shared-object) (nn c-file)
             "-lfaiss")))))
