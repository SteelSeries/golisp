;;; -*- mode: Scheme -*-

(context "nil"

         ()

         (it "can parse and compare nil"
                   (assert-nil '())
                   (assert-not-nil '(()))
                   (assert-not-nil '(()()))
                   (assert-error ())))
