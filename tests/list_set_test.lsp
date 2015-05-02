(describe union
          (== (union '(1 2 3) '(3 4 5)) '(1 2 3 4 5))
          (== (union '() '(1)) '(1))
          (== (union '() '()) '()))

(describe intersection
          (== (intersection '(1 2 3) '(2 3 4 5)) '(2 3))
          (== (intersection '() '(1 2)) '())
          (== (intersection '(1) '(2)) '())
          (== (intersection '() '()) '())
          (== (intersection '(18 31 4 20 14 36 27 33 15 38) '(26 31 32 33 21 9 7 22)) '(31 33)))
