;;; -*- mode: Scheme -*-

(define x 5)

(define (f a b . c)
  (cons a (cons b c)))

(context "define"

         ()

         (it "scopes properly"
             (assert-eq ((lambda (y)
                           (define x 10)
                           x)
                         1)
                        10))

         (it "supports var-args"
                   (assert-eq (f 1 2 3 4 5)
                              '(1 2 3 4 5))
                   (assert-eq ((lambda (a . b)
                                 (apply a b))
                               + 1 2 3)
                              6))

         (it "errors appropriately"
                   (assert-error (define "x" 4))
                   (assert-error (define ("x") 4))
                   (assert-error (define (+ x y) 42))))
