;;; -*- mode: Scheme -*-

(context "equality"

         ()

         (it works
             (assert-false (eq? (list) 42))
             (assert-false (eq? 42 (list)))
             (assert-false (eq? 42 "42"))
             (assert-false (eq? '(1 2) '(1 2 3)))))
