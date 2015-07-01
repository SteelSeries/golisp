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

(describe filter-errors
          (assert-error (filter 5 '())) ;1st arg must be a function
          (assert-error (filter even? 5)) ;2nd arg must be a list
          (assert-error (filter + '(1 2)))) ;1st arg must be a predicate

(describe remove
          (assert-eq (remove even? '())
                     '())
          (assert-eq (remove even? '(2 4 6))
                     '())
          (assert-eq (remove even? '(1 3 5))
                     '(1 3 5))
          (assert-eq (remove even? '(1 2 3 4 5 6))
                     '(1 3 5)))


(describe remove-errors
          (assert-error (remove 5 '())) ;1st arg must be a function
          (assert-error (remove even? 5)) ;2nd arg must be a list
          (assert-error (remove + '(1 2)))) ;1st arg must be a predicate
