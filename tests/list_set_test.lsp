(describe union
          (== (union '(1 2 3) '(3 4 5)) '(1 2 3 4 5))
          (== (union '() '(1)) '(1))
          (== (union '() '()) '()))

(describe intersection
          (== (intersection '(1 2 3) '(2 3 4 5)) '(2 3))
          (== (intersection '() '(1 2)) '())
          (== (intersection '(1) '(2)) '())
          (== (intersection '() '()) '()))
