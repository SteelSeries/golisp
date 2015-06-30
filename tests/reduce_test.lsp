;;; -*- mode: Scheme -*-

(describe reduce-with-lambda
          (assert-eq (reduce (lambda (acc item) 0 (+ acc item)) 0 '(1 2 3))
                     6))

(describe reduce-with-primitive
          (assert-eq (reduce + 0 '(1 2 3))
                     6))

(describe reduce-building-a-list
          (assert-eq (reduce (lambda (l i) (cons i l)) '() '(1 2 3 4))
                     '(4 3 2 . 1))
          (assert-eq (reduce list '() '(1 2 3 4))
                     '(((1 2) 3) 4)))

(describe reduce-errors
          (assert-error (reduce r r '(1 2))) ;initial arg must be a function
          (assert-error (reduce + 0 1))) ;last/3rd arg must be a list
