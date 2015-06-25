;;; -*- mode: Scheme -*-

(describe arithmetic
          (assert-eq (+ 5.0 5.0)
                     10.0)
          (assert-eq (- 10.0 7.0)
                     3.0)
          (assert-eq (- 7.0 10.0)
                     -3.0)
          (assert-eq (* 2.0 4.0)
                     8.0)
          (assert-eq (/ 25.0 5.0)
                     5.0))

(define fx 2.2)
(define fy 8.8)
(define fz 7.7)

(describe conditions
          (assert-true (< fx fy))
          (assert-false (< fy fz))
          (assert-false (> fx fy))
          (assert-true (> fz fx))
          (assert-true (<= fx 2.2))
          (assert-true (>= fz 7)))

(describe conversions
          (assert-eq (float 5)
                     5.0)
          (assert-eq (integer 5.6)
                     5))

