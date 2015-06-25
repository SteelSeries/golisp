;;; -*- mode: Scheme -*-

(define x 5)
(describe local-function-scope
          (assert-eq ((lambda (y)
                        (define x 10)
                        x)
                      1)
                     10))

(define (f a b . c)
  (cons a (cons b c)))

(describe var-args
          (assert-eq (f 1 2 3 4 5)
                     '(1 2 3 4 5))
          (assert-eq ((lambda (a . b)
                        (apply a b))
                      + 1 2 3)
                     6))
