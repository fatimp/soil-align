(defun do-all()
  (handler-case
      (asdf:load-system :soil-align/tests)
    (error ()
      (uiop:quit 1)))
  (uiop:quit
   (if (uiop:call-function "soil-align/tests:run-tests")
       0 1)))

(do-all)
