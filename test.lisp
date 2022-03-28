(letrec
    ((factorial
      (lambda (x)
        (if (zero? x)
            1
            (* x ((ref factorial) (- x 1)))))))
  ((ref factorial) 5))

(letrec
    ((f
      (lambda (x)
        (if (zero? x)
            0
            (+ x ((ref f) (- x 1)))))))
  ((ref f) 1))

(labels
    ((f0 (code (x) (f) (if (zero? x) 0 (+ x (f (- x 1)))))))
  (letrec
      ((f (closure f0 f)))
    (f 1)))
