(labels
    ((add (code
          (a b)
          (let ((z 1))
            (+ a (+ b z))))))
  (let ((x 32))
    (add x 1)))
