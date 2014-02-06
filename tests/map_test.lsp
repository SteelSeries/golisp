(define (foo a)
    (lambda (x) (* a x)))

(describe map-with-constructed-lambda
    (== (map (foo 5) '(1 2 3)) '(5 10 15)))
    
(describe map-with-constructed-lambda
    (== (map (lambda (x) (* x 5)) '(1 2 3)) '(5 10 15)))

(describe map-with-prim
    (== (map car '((1 2) (3 4) (5 6))) '(1 3 5)))