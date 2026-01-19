(in-package :soil-align/tests)

(def-suite stuff :description "Different aspects of soil-align")

(defun run-tests ()
  (every #'identity
         (mapcar (lambda (suite)
                   (let ((status (run suite)))
                     (explain! status)
                     (results-status status)))
                 '(stuff))))

(in-suite stuff)

(test pca
  (loop repeat 100
        ;; Number of samples
        for samples = (+ 1000 (random 1000))
        ;; Number of independent variables
        for indep   = (+ 50 (random 100))
        for descr   = (make-array (list samples util:+descriptor-length+)
                                  :element-type 'single-float
                                  :initial-element 0.0)
        do
           (loop for i below samples do
             (loop for j below indep do
               (setf (aref descr i j) (random 1.0))))
           (multiple-value-bind (trans means)
               (pca:fit-pca descr 0.99)
             (is (<= (array-dimension trans 0)
                     (1+ indep)))
             (let* ((descr-pcad (pca:transform-pca descr trans means))
                    (descr-back (pca:invert-pca descr-pcad trans means)))
               (is-true (array-approx-p descr descr-back :rtol 1f-1))))))

(test ransac
  (flet ((random-vec ()
           (make-array 3
                       :element-type 'single-float
                       :initial-contents
                       (loop repeat 3 collect (random 100.0)))))
    (loop repeat 50
          ;; Number of inliers
          for inliers = (+ 2000 (random 1000))
          ;; Number of outliers
          for outliers = (+ 200 (random 100))
          ;; A simple transform
          for translation = (random-vec)
          for matches-good = (loop repeat inliers
                                   for p1 = (random-vec)
                                   for p2 = (map '(vector single-float) #'+
                                                 p1 translation)
                                   collect (cons p1 p2))
          for matches-bad = (loop repeat outliers
                                  collect (cons (random-vec)
                                                (random-vec)))
          for matches = (append matches-good matches-bad)
          for transform = (tran:ransac #'tran:rigid-transform-fit matches
                                       :max-iter 1000 :err 10.0)
          do
             (loop for i below 3 do
               (is-true
                (approxp (aref transform i 3)
                         (aref translation i)
                         :rtol 1f-2)))
             (loop for i below 3 do
               (loop for j below 3 do
                 (let ((x (aref transform i j)))
                   (if (= i j)
                       (is (approxp x 1.0))
                       (< (abs x) 1f-5))))))))
