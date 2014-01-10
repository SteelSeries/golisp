(describe interval
          (== (interval 1 1) '(1))
          (== (interval 1 2) '(1 2))
          (== (interval 1 5) '(1 2 3 4 5))
          (== (interval 3 2) '()))
