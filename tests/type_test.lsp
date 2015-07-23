;;; -*- mode: Scheme -*-

(define a 4)
(define foo (lambda (x) (+ x x)))

(context "types"

         ()
         
         (it type-test
                   (assert-true (list? '(list 1 2 3)))
                   (assert-false (list? a))
                   (assert-false (pair? (acons 'a 1)))
                   (assert-false (list? (acons 'a 1)))
                   (assert-true (alist? (acons 'a 1)))
                   (assert-error (pair?))
                   (assert-true (nil? '()))
                   (assert-false (nil? a))
                   (assert-true (notnil? a))
                   (assert-false (notnil? '()))
                   (assert-true (string? "bar"))
                   (assert-false (string? 3))
                   (assert-true (symbol? 'a))
                   (assert-false (symbol? "bar"))
                   (assert-true (integer? a))
                   (assert-true (number? a))
                   (assert-true (float? 3.2))
                   (assert-true (number? 3.2))
                   (assert-false (float? ""))
                   (assert-false (integer? "bar"))
                   (assert-false (number? "bar"))
                   (assert-true (function? foo))
                   (assert-false (function? 1))
                   (assert-true (frame? {a: 1}))
                   (assert-false (frame? 1))
                   (assert-true (bytearray? [1 2]))
                   (assert-false (bytearray? 1)))

)
