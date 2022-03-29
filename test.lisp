(letrec
    ((fib
      (lambda (n)
        (if (zero? n)
            0
            (if (= n 1)
                1
                (+ ((ref fib) (- n 1)) ((ref fib) (- n 2))))))))
  ((ref fib) 2))
