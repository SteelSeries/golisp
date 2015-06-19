(describe interval
          (== (interval 1 1) '(1))
          (== (interval 1 2) '(1 2))
          (== (interval 1 5) '(1 2 3 4 5))
          (== (interval 1 5) '(1 2 3 4 5))))

(describe interval-with-step
          (== (interval 1 4 1) '(1 2 3 4))
          (== (interval 1 9 2) '(1 3 5 7 9))
          (== (interval 1 10 2) '(1 3 5 7 9))
          (== (interval 0 100 10) '(0 10 20 30 40 50 60 70 80 90 100)))

(describe reverse-interval
          (== (interval 3 1) '(3 2 1))
          (== (interval 10 1 -2) '(10 8 6 4 2)))

(describe single-arg-interval
          (== (interval 1) '(1))
          (== (interval 10) '(1 2 3 4 5 6 7 8 9 10)))

