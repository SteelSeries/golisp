(describe lambda
          (== ((lambda () 42)) 42)
          (== ((lambda (x) (+ x 3)) 6) 9)
          (== ((lambda (x) (* x x)) 6) 36))

(define foo (lambda (x)
              (if (== x 0)
                  0
                (+ x (foo (- x 1))))))

(describe named-lambda
          (== (foo 0) 0)
          (== (foo 1) 1)
          (== (foo 2) 3)
          (== (foo 3) 6)
          (== (foo 4) 10))

(define (bar x)
  (if (== x 0)
      0
    (+ x (bar (- x 1)))))

(describe function
          (== (bar 0) 0)
          (== (bar 1) 1)
          (== (bar 2) 3)
          (== (bar 3) 6)
          (== (bar 4) 10))
