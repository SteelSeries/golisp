;;; -*- mode: Scheme -*-

(context "reduce"

        ()

        (it reduce-with-lambda
            (assert-eq (reduce (lambda (acc item) 0 (+ acc item)) 0 '(1 2 3))
                       6))

        (it reduce-with-primitive
            (assert-eq (reduce + 0 '(1 2 3))
                       6))

        (it reduce-building-a-list
            (assert-eq (reduce (lambda (l i) (cons i l)) '() '(1 2 3 4))
                       '(4 3 2 . 1))
            (assert-eq (reduce list '() '(1 2 3 4))
                       '(((1 2) 3) 4)))

        (it reduce-lengths
            (assert-eq (reduce-left + 0 '()) 0)
            (assert-eq (reduce-left + 0 '(1)) 1)
            (assert-eq (reduce-left + 0 '(1 2)) 3)
            (assert-eq (reduce-right + 0 '()) 0)
            (assert-eq (reduce-right + 0 '(1)) 1)
            (assert-eq (reduce-right + 0 '(1 2)) 3))

        (it reduce-errors
            (assert-error (reduce r r '(1 2))) ;initial arg must be a function
            (assert-error (reduce + 0 1))) ;last/3rd arg must be a list

        (it reduce-direction
            (assert-eq (reduce-left list '() '(1 2 3 4))
                       '(((1 2) 3) 4))

            (assert-eq (reduce-right list '() '(1 2 3 4))
                       '(1 (2 (3 4)))))

        (it fold
            (assert-eq (fold-left list '() '(1 2 3 4))
                       '((((() 1) 2) 3) 4))

            (assert-eq (fold-right list '() '(1 2 3 4))
                       '(1 (2 (3 (4 ()))))))
)
