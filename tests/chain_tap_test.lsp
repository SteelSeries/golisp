;;; -*- mode: Scheme -*-

(describe chain
          (assert-eq (-> 1)
                     1)
          (assert-eq (-> 1 (+ 2))
                     3)
          (assert-eq (-> 1 (+ 2) (* 3))
                     9)
          (assert-eq (-> 1 (+ 2) str)
                     "3"))

(describe parallel-chain
          (assert-eq (=> 1 (* 2) str)
                     1)
          (assert-eq (let* ((a 1)
                            (foo (lambda (x) (set! a x))))
                       (=> 4 foo)
                       a)
                     4))
