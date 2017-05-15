;;; -*- mode: Scheme -*-

(context arithmetic

         ()

         (it "can add"
             (assert-eq (+ 5.0 5.0)
                        10.0))

         (it "can subtract"
             (assert-eq (- 10.0 7.0)
                        3.0))

         (it "can subtract to a negative"
             (assert-eq (- 7.0 10.0)
                        -3.0))

         (it "can multiply"
             (assert-eq (* 2.0 4.0)
                        8.0))

         (it "can divide"
             (assert-eq (/ 25.0 5.0)
                        5)))

(define fx 2.2)
(define fy 8.8)
(define fz 7.7)

(context conditions

         ()

         (it "can compare"
             (assert-true (< fx fy))
             (assert-false (< fy fz))
             (assert-false (> fx fy))
             (assert-true (> fz fx))
             (assert-true (<= fx 2.2))
             (assert-true (>= fz 7))))

(context conversions

         ()

         (it "can convert an integer"
             (assert-eq (float 5)
                        5.0))

         (it "rejects converting a string to float"
             (assert-error (float "5")))

         (it "truncates when converting to an integer"
             (assert-eq (integer 5.6)
                        5))

         (it "rejects converting a string to integer"
             (assert-error (integer "5.3"))))

