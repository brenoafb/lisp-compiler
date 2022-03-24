(labels ((f0 (code (y) (x) (+ x y))))
  (let ((x 9)) ((closure f0 x) 11)))
