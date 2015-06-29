;;; -*- mode: Scheme -*-

(describe basic-cond
          (assert-eq (cond (#t 1)
                           (#f 2))
                     1)
          (assert-eq (cond (#f 1)
                           (#t 2))
                     2)
          (assert-eq (cond (#f 1)
                           (#f 2)
                           (else 3))
                     3)

          (assert-error (cond #t 1
                              (#t 2))))

(describe multi-expr-cond
          (assert-eq (cond (#f 1)
                           (#t 1 2 3))
                     3)
          (assert-eq (cond (#f 1 2 3)
                           (#f 4 5 6)
                           (else 7 8 9))
                     9))
