;;; -*- mode: Scheme -*-

(define x 4)

(define y 5)

(context "setting"

         ()
         
         (it "set!-in-global-context"
             (assert-eq x
                        4)
             (assert-eq (begin
                          (set! x 10)
                          x)
                        10)
             (assert-eq x
                        10))

         (it "set!-in-local-context"
             (assert-eq y
                        5)
             (assert-eq (let ((y 2))
                          (set! y 15)
                          y)
                        15)
             (assert-eq y
                        5))

         (it "set-car!"
             (assert-eq (let ((pair '(a b)))
                          (set-car! pair 1)
                          (car pair))
                        1))

         (it "set-cdr!"
             (assert-eq (let ((pair '(a b)))
                          (set-cdr! pair 1)
                          (cdr pair))
                        1))

         (it "set-nth!"
             (assert-eq (let ((l '(a b c d)))
                          (set-nth! 3 l 1)
                          (nth 3 l))
                        1))
)
