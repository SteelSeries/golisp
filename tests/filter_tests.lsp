;;; -*- mode: Scheme -*-

(describe filter
          (assert-eq (filter even? '())
                     '())
          (assert-eq (filter even? '(1 3 5))
                     '())
          (assert-eq (filter even? '(2 4 6))
                     '(2 4 6))
          (assert-eq (filter even? '(1 2 3 4 5 6))
                     '(2 4 6)))

(describe remove
          (assert-eq (remove even? '())
                     '())
          (assert-eq (remove even? '(2 4 6))
                     '())
          (assert-eq (remove even? '(1 3 5))
                     '(1 3 5))
          (assert-eq (remove even? '(1 2 3 4 5 6))
                     '(1 3 5)))
