(define x 4)

(describe set-in-global-context
          (== x 4)
          (== (begin (set! 'x 10)
                     x)
              10)
          (== x 10))

(define y 5)

(describe set-in-local-context
          (== y 5)
          (== (let ((y 2))
                (set! 'y 15)
                y)
              15)
          (== y 5))
