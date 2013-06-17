(define a 4)
(define foo (lambda (x) (+ x x)))

(describe type-test
    (== (list? '(list 1 2 3)) #t)
    (== (list? a)             #f)
    (== (pair? (acons 'a 1))  #t)
    (== (pair? #f)            #f)
    ; nil? on '() fails
    (== (nil? '())            #t)
    (== (nil? a)              #f)
    (== (notnil? a)           #t)
    ; notnil? on '() fails
    (== (notnil? '())         #f)
    (== (string? "bar")       #t)
    (== (string? 3)           #f)
    (== (symbol? a)           #t)
    (== (symbol? "bar")       #f)
    (== (number? a)           #t)
    (== (number? "bar")       #f)
    (== (function? foo)       #t)
    (== (function? 1)         #f))