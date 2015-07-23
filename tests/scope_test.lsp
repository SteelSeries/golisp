;;; -*- mode: Scheme -*-

(define a 5)

(define (foo a)
  (lambda (x)
    (+ a x)))

(context "scope"

         ()
         
         (it global-env
             (assert-eq a
                        5))

         (it lambda-env
             (assert-eq ((foo 1) 5)
                        6)
             (assert-eq ((foo 2) 5)
                        7)
             (assert-eq ((foo 10) 7)
                        17)))
