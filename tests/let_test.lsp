;;; -*- mode: Scheme -*-

(describe simple-let
          (assert-nil (let ()))
          (assert-eq (let ()
                       42)
                     42))

(describe let-with-multiple-expr-body
          (assert-eq (let ()
                       1
                       2)
                     2))

(describe let-bindings
          (assert-eq (let* ((x 1)
                            (y 2))
                       (+ x y))
                     3)
          (let ((x 2))
            (assert-eq (let ((x 1)
                             (y (+ x 1)))
                         y)
                       3)))


(describe let-binding-scope
          (assert-nil (begin (let ((zz 2)) zz)
                             zz)))

(describe named-let
          (assert-eq (let loop
                         ((numbers '(3 -2 1 6 -5))
                          (nonneg '())
                          (neg '()))
                       (cond ((null? numbers)
                              (list nonneg neg))
                             ((>= (car numbers) 0)
                              (loop (cdr numbers)
                                    (cons (car numbers) nonneg)
                                    neg))
                             (else
                              (loop (cdr numbers)
                                    nonneg
                                    (cons (car numbers) neg)))))
                     '((6 1 3) (-5 -2))))
