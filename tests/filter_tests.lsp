(describe filter
          (== (filter even? '()) '())
          (== (filter even? '(1 3 5)) '())
          (== (filter even? '(2 4 6)) '(2 4 6))
          (== (filter even? '(1 2 3 4 5 6)) '(2 4 6)))