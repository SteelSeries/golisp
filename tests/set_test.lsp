;;; -*- mode: Scheme -*-

(define x 4)

(describe set!-in-global-context
          (assert-eq x
                     4)
          (assert-eq (begin
                       (set! x 10)
                       x)
                     10)
          (assert-eq x
                     10))

(define y 5)

(describe set!-in-local-context
          (assert-eq y
                     5)
          (assert-eq (let ((y 2))
                       (set! y 15)
                       y)
                     15)
          (assert-eq y
                     5))

(describe set-car!
          (assert-eq (let ((pair '(a b)))
                       (set-car! pair 1)
                       (car pair))
                     1))

(describe set-cdr!
          (assert-eq (let ((pair '(a b)))
                       (set-cdr! pair 1)
                       (cdr pair))
                     1))

(describe set-nth!
          (assert-eq (let ((l '(a b c d)))
                       (set-nth! l 3 1)
                       (nth l 3))
                     1))
