;;; -*- mode: Scheme -*-

(define (test-func x)
  (case x
    ((0) "zero")
    ((1) "one")
    ((2) "two")
    ((3) "three")
    (else "unknown")))

(define (complex-func x)
  (let ((y 1))
    (case x
      ((0) (set! y 2)
       (+ y 1))
      ((1) (set! y 5)
       (+ y 2))
      (else (set! y 10)
            (+ y 16)))))

(define (multi-test-func x)
  (case x
    ((0) "none")
    ((1) "one")
    ((2) "a couple")
    ((3 4 5) "a few")
    ((6 7 8) "some")
    (else "many")))

(context "case"

         ()

         (it case
                   (assert-eq (test-func 0)
                              "zero")
                   (assert-eq (test-func 1)
                              "one")
                   (assert-eq (test-func 2)
                              "two")
                   (assert-eq (test-func 3)
                              "three")
                   (assert-eq (test-func 5)
                              "unknown")

                   (assert-error (case 5
                                   4 4))
                   (assert-error (case 5 (4 4)))
                   (assert-error (case 5 (foo 4))))

         (it complex-case
                   (assert-eq (complex-func 0)
                              3)
                   (assert-eq (complex-func 1)
                              7)
                   (assert-eq (complex-func 42)
                              26))

         (it multi-case
                   (assert-eq (multi-test-func 0)
                              "none")
                   (assert-eq (multi-test-func 1)
                              "one")
                   (assert-eq (multi-test-func 2)
                              "a couple")
                   (assert-eq (multi-test-func 3)
                              "a few")
                   (assert-eq (multi-test-func 4)
                              "a few")
                   (assert-eq (multi-test-func 5)
                              "a few")
                   (assert-eq (multi-test-func 6)
                              "some")
                   (assert-eq (multi-test-func 7)
                              "some")
                   (assert-eq (multi-test-func 8)
                              "some")
                   (assert-eq (multi-test-func 9)
                              "many")))

