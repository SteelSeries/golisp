(define x 4)

(describe set!-in-global-context
          (== x 4)
          (== (begin (set! x 10)
                     x)
              10)
          (== x 10))

(define y 5)

(describe set!-in-local-context
          (== y 5)
          (== (let ((y 2))
                (set! y 15)
                y)
              15)
          (== y 5))

(describe set-car!
          (== (let ((pair '(a b)))
                (set-car! pair 1)
                (car pair))
              1))

(describe set-cdr!
          (== (let ((pair '(a b)))
                (set-cdr! pair 1)
                (cdr pair))
              1))

(describe set-nth!
          (== (let ((l '(a b c d)))
                (set-nth! l 3 1)
                (nth l 3))
              1))
