(define x 5)
(describe local-function-scope
          (== ((lambda (y) (define x 10) x) 1)
              10))
