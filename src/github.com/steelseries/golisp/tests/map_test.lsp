(define (foo a)
    (lambda (x) (* a x)))

(describe map-test
    (== (map (foo 5) '(1 2 3)) '(5 10 15)))