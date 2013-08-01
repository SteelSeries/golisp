(define x 2.2)
(define y 8.8)
(define z 7.7)

(describe arithmetic-test
    (== (+ 5.0 5.0) 10.0)
    (== (- 10.0 7.0) 3.0)
    (== (* 2.0 4.0)  8.0)
    (== (/ 25.0 5.0) 5.0))

(describe condition-test
    (== (< x y)  #t)
    (== (< y z)  #f)
    (== (> x y)  #f)
    (== (> z x)  #t)
    (== (<= x 2) #t)
    (== (>= z 7) #t))
