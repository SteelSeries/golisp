;;; -*- mode: Scheme -*-

(define xx 2)
(define y 8)
(define z 7)

(context "math"

         ()
         
         (it arithmetic-test
             (assert-eq (+ 5 5)
                        10)
             (assert-eq (- 10 7)
                        3)
             (assert-eq (* 2 4)
                        8)
             (assert-eq (/ 25 5)
                        5)
             (assert-eq (quotient 25 5)
                        5)
             (assert-eq (% 7 5)
                        2)
             (assert-eq (modulo 7 5)
                        2))

         (it subtraction-going-negative
             (assert-eq (- 5 9)
                        -4))

         (it condition-test
             (assert-true (< xx y))
             (assert-false (< y z))
             (assert-error (< "a" 5))
             (assert-error (< 5 "a"))
             
             (assert-false (> xx y))
             (assert-true (> z xx))
             (assert-error (> "a" 5))
             (assert-error (> 5 "a"))

             (assert-true (<= xx 2))
             (assert-error (<= "a" 5))
             (assert-error (<= 5 "a"))

             (assert-true (>= z 7))
             (assert-error (>= "a" 5))
             (assert-error (>= 5 "a"))

             (assert-false (!= xx xx))
             (assert-false (!= 2 xx))
             (assert-true (!= 2 3))
             (assert-true (! #f))
             (assert-false (! #t))
             (assert-true (not #f))
             (assert-false (not #t))
             (assert-false (not '(a b)))
             (assert-true (not '()))
             (assert-false (or #f #f))
             (assert-true (or #f #f #t))
             (assert-false (and #t #f #t))
             (assert-true (and #t #t #t))
             (assert-true (or (> xx z) (> y z)))
             (assert-false (and (> xx z) (> y z)))
             (assert-true (even? 2))
             (assert-false (even? 3))
             (assert-true (odd? 3))
             (assert-false (odd? 2)))

         (it "and/or work for control flow with short circuiting"
             (assert-eq 5 (and (> 4 2) (+ 4 1)))
             (assert-eq #f (and (< 4 2) (+ 4 1)))
             (assert-eq #t (or (> 4 2) (+ 4 1)))
             (assert-eq 5 (or (< 4 2) (+ 4 1))))

         (it int-min
             (assert-eq (min '(1 2))
                        1)
             (assert-eq (min '(3 4 2 8 8 6 1))
                        1))

         (it float-min
             (assert-eq (min '(1.3 2.0))
                        1.3)
             (assert-eq (min '(3 4.8 2 8 8.3 6 1))
                        1.0))

         (it int-max
             (assert-eq (max '(1 2))
                        2)
             (assert-eq (max '(3 4 2 8 8 6 1))
                        8))

         (it float-max
             (assert-eq (max '(1.3 2.2))
                        2.2)
             (assert-eq (max '(3 4.8 2 8 8.3 6 1))
                        8.3))

         (it floor
             (assert-eq (floor 3.4)
                        3.0)
             (assert-eq (floor -3.4)
                        -4.0)
             (assert-eq (floor 3)
                        3.0))

         (it ceiling
             (assert-eq (ceiling 3.4)
                        4.0)
             (assert-eq (ceiling -3.4)
                        -3.0)
             (assert-eq (ceiling 3)
                        3.0))

         (it general-math-errors
             (assert-error (/ 3 0))
             (assert-error (% 3.5 6))
             (assert-error (& 6 4.7))
             (assert-error (min '(1 d)))
             (assert-error (max 5.4 i))
             (assert-error (floor 'd))
             (assert-error (celing 'd))
             (assert-error (abs "hi"))
             (assert-error (zero? 'zero))
             (assert-error (positive? +))
             (assert-error (negative '-))
             (assert-error (even? "j"))
             (assert-error (odd 'r))
             (assert-error (sign 's)))
)
