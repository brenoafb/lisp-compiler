(labels ((f0 (code (y) (x) (if (= x 0) 1 y))))
  (let ((x 0)) ((closure f0 x) 3)))
