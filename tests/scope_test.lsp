(define a 5)

(define (foo a)
  (lambda (x) (+ a x)))

(describe global-env
          (== a 5))

(describe lambda-env
          (== ((foo 1) 5) 6)
          (== ((foo 2) 5) 7)
          (== ((foo 10) 7) 17))
