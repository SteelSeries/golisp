;;; -*- mode: Scheme -*-

(context "chaining"

         ()

         (it "ripples through"
             (assert-eq (-> 1)
                        1)
             (assert-eq (-> 1 (+ 2))
                        3)
             (assert-eq (-> 1 (+ 2) (* 3))
                        9)
             (assert-eq (-> 1 (+ 2) str)
                        "3")))

(context "parallel chaining"

         ()

         (it "doesn't ripple"
             (assert-eq (=> 1 (* 2) str)
                        1)
             (assert-eq (let* ((a 1)
                               (foo (lambda (x) (set! a x))))
                          (=> 4 foo)
                          a)
                        4)))
