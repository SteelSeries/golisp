;;; -*- mode: Scheme -*-

(context "let"

         ()
         
         (it simple-let
             (assert-nil (let ()))
             (assert-eq (let ()
                          42)
                        42)
             (assert-error (let 5 42))    ;non-list bindings
             (assert-error (let ((5 1)) 42)) ;non-symbol binding name
             (assert-error (let (5 3) 42))) ;non-pair binding

         (it let-with-multiple-expr-body
             (assert-eq (let ()
                          1
                          2)
                        2))

         (it let-let*-scope
             (assert-eq (let* ((x 1)
                               (y 2))
                          (+ x y))
                        3)
             (let ((x 2))
               (assert-eq (let ((x 1)
                                (y (+ x 1)))
                            y)
                          3)))


         (it let-binding-scope
             (assert-nil (begin (let ((zz 2)) zz)
                                zz)))

         (it named-let
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
                        '((6 1 3) (-5 -2)))

             (assert-error (let 4 ((x 1)) (+ 1 2))) ;non-symbol name
             (assert-error (let name "hi" (+ 1 2))) ;non-list bindings
             (assert-error (let name ((4 1)) (+ 1 2)))) ;non-symbol binding name
)
