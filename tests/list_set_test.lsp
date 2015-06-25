;;; -*- mode: Scheme -*-

(describe union
          (assert-eq (union '(1 2 3) '(3 4 5))
                     '(1 2 3 4 5))
          (assert-eq (union '() '(1))
                     '(1))
          (assert-eq (union '() '())
                     '())
          ;; union should not affect the base list parameters
          (begin
            (define a '(1 2 3))
            (define b '(4 5))
            (union a b)
            (assert-eq a
                       '(1 2 3))
            (assert-eq b
                       '(4 5)))
          (begin
            (define a '(1 2 3))
            (define b '(4 5))
            (union '() a b)
            (assert-eq a
                       '(1 2 3))
            (assert-eq b
                       '(4 5))))

(describe intersection
          (assert-eq (intersection '(1 2 3) '(2 3 4 5))
                     '(2 3))
          (assert-eq (intersection '() '(1 2))
                     '())
          (assert-eq (intersection '(1) '(2))
                     '())
          (assert-eq (intersection '() '())
                     '())
          (assert-eq (intersection '(18 31 4 20 14 36 27 33 15 38) '(26 31 32 33 21 9 7 22))
                     '(31 33))
          ;; intersection should not affect the base list parameters
          (begin
            (define a '(1 2 3 4 5))
            (define b '(4 3 2))
            (intersection a b)
            (assert-eq a
                       '(1 2 3 4 5))
            (assert-eq b
                       '(4 3 2))))
