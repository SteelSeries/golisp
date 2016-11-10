;;; -*- mode: Scheme -*-

(define foo (lambda (x)
              (if (assert-eq x 0)
                  0
                  (+ x (foo (- x 1))))))

(define named-foo (named-lambda (named-foo x)
                    (if (eq? x 0)
                        0
                        (+ x (named-foo (- x 1))))))

(define (bar x)
  (if (eq? x 0)
      0
      (+ x (bar (- x 1)))))


(context "lambda"

         ()
         
         (it lambda
                   (assert-eq ((lambda () 42))
                              42)
                   (assert-eq ((lambda (x) (+ x 3)) 6)
                              9)
                   (assert-eq ((lambda (x) (* x x)) 6)
                              36)

                   (assert-error (lambda x (+ 1 2))))

         (it named-lambda
                   (assert-eq (named-foo 0)
                              0)
                   (assert-eq (named-foo 1)
                              1)
                   (assert-eq (named-foo 2)
                              3)
                   (assert-eq (named-foo 3)
                              6)
                   (assert-eq (named-foo 4)
                              10)

                   (assert-error (named-lambda x (+ 1 2)))
                   (assert-error (named-lambda ("h" x) (+ 1 2))))

         (it function
                   (assert-eq (bar 0)
                              0)
                   (assert-eq (bar 1)
                              1)
                   (assert-eq (bar 2)
                              3)
                   (assert-eq (bar 3)
                              6)
                   (assert-eq (bar 4)
                              10)))
