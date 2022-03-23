(labels ((f0 (code () (x y) (+ x y))))
  (let ((x 5) (y 2)) ((closure f0 x y))))
