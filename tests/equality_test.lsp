;;; -*- mode: Scheme -*-

(describe equals
          (assert-false (eq? (list) 42))
          (assert-false (eq? 42 (list)))
          (assert-false (eq? (alist '((a.1))) 42))
          (assert-false (eq? (car (alist '((a.1)))) 42))
          (assert-false (eq? 42 "42"))
          (assert-false (eq? (alist '((a.1))) (alist '((a.1) (b.2)))))
          (assert-false (eq? '(1 2) '(1 2 3))))
