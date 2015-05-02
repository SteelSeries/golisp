(describe union
          (== (union '(1 2 3) '(3 4 5)) '(1 2 3 4 5))
          (== (union '() '(1)) '(1))
          (== (union '() '()) '()))

(describe intersection
          (== (intersection '(1 2 3) '(2 3 4 5)) '(2 3))
          (== (intersection '() '(1 2)) '())
          (== (intersection '(1) '(2)) '())
          (== (intersection '() '()) '())
          (== (intersection '(18 31 4 20 14 36 27 33 15 38) '(26 31 32 33 21 9 7 22)) '(31 33))
          ;; intersection should not affect the base list parameters
          (begin
          	(define a '(1 2 3 4 5))
          	(define b '(4 3 2))
          	(intersection a b)
          	(and (== a '(1 2 3 4 5)) (== b '(4 3 2)))
          )

          )
