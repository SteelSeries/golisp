(define x 5)
(describe local-function-scope
          (== ((lambda (y) (define x 10) x) 1)
              10))

(define (f a b . c)
  (cons a (cons b c)))

(describe var-args
          (== (f 1 2 3 4 5) '(1 2 3 4 5))
          (== ((lambda (a . b) (apply a b)) + 1 2 3) 6))
