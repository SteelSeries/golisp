;;; -*- mode: Scheme -*-

(context "interval"

         ()
         
         (it "makes simple increasing sequences"
             (assert-eq (interval 1 1) '(1))
             (assert-eq (interval 1 2) '(1 2))
             (assert-eq (interval 1 5) '(1 2 3 4 5))
             (assert-eq (interval 1 5) '(1 2 3 4 5)))

         (it "lets you use a step with a sign in the direction of the interval"
             (assert-eq (interval 1 4 1) '(1 2 3 4))
             (assert-eq (interval 1 9 2) '(1 3 5 7 9))
             (assert-eq (interval 1 10 2) '(1 3 5 7 9))
             (assert-eq (interval 0 100 10) '(0 10 20 30 40 50 60 70 80 90 100))
             (assert-error (interval 1 10 -2))
             (assert-error (interval 1 10 5.3)))

         (it "supports decreasing sequences, with optional step"
             (assert-eq (interval 3 1) '(3 2 1))
             (assert-eq (interval 10 1 -2) '(10 8 6 4 2))
             (assert-error (interval 10 1 2)))

         (it "supports a simpole version for 1...n sequences"
             (assert-eq (interval 1) '(1))
             (assert-eq (interval 10) '(1 2 3 4 5 6 7 8 9 10))))

