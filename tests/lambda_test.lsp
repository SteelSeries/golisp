(describe lambda
          (== ((lambda () 42)) 42)
          (== ((lambda (x) (+ x 3)) 6) 9)
          (== ((lambda (x) (* x x)) 6) 36))

(define foo (lambda (x)
              (if (== x 0)
                  0
                (+ x (foo (- x 1))))))

(define named-foo (named-lambda (named-foo x)
                           (if (eq? x 0)
                               0
                               (+ x (named-foo (- x 1))))))
(describe named-lambda
         (== (named-foo 0) 0)
          (== (named-foo 1) 1)
          (== (named-foo 2) 3)
          (== (named-foo 3) 6)
          (== (named-foo 4) 10))

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
