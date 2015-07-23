;;; -*- mode: Scheme -*-

(context "begin"

         ()

         (it "returns the last thing evaluated"
             (assert-eq (begin 4)
                        4)
             (assert-eq (begin 1 2)
                        2)))
