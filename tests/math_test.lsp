(define xx 2)
(define y 8)
(define z 7)

(describe arithmetic-test
    (== (+ 5 5) 10)
    (== (- 10 7) 3)
    (== (* 2 4)  8)
    (== (/ 25 5) 5)
    (== (quotient 25 5) 5)
    (== (% 7 5)  2)
    (== (modulo 7 5)  2))

(describe subtraction-going-negative
          (== (- 5 9) -4))

(describe condition-test
    (== (< xx y)  #t)
    (== (< y z)  #f)
    (== (> xx y)  #f)
    (== (> z xx)  #t)
    (== (<= xx 2) #t)
    (== (>= z 7) #t)
    (== (!= xx xx) #f)
    (== (!= 2 xx) #f)
    (== (!= 2 3) #t)
    (== (! #f)   #t)
    (== (! #t)   #f)
    (== (not #f)   #t)
    (== (not #t)   #f)
    (== (not '(a b)) #f)
    (== (not '()) #t)
    (== (or #f #f)     #f)
    (== (or #f #f #t)  #t)
    (== (and #t #f #t) #f)
    (== (and #t #t #t) #t)
    (== (or (> xx z) (> y z))  #t)
    (== (and (> xx z) (> y z))  #f)
    (== (even? 2) #t)
    (== (even? 3) #f)
    (== (even? "hi") #f)
    (== (odd? 3) #t)
    (== (odd? 2) #f)
    (== (odd? "hi") #f))

(describe int-min
          (== (min '(1 2)) 1)
          (== (min '(3 4 2 8 8 6 1)) 1))

(describe float-min
          (== (min '(1.3 2.0)) 1.3)
          (== (min '(3 4.8 2 8 8.3 6 1)) 1.0))

(describe int-max
          (== (max '(1 2)) 2)
          (== (max '(3 4 2 8 8 6 1)) 8))

(describe float-max
          (== (max '(1.3 2.2)) 2.2)
          (== (max '(3 4.8 2 8 8.3 6 1)) 8.3))

(describe floor
          (== (floor 3.4) 3.0)
          (== (floor -3.4) -4.0)
          (== (floor 3) 3.0))

(describe ceiling
          (== (ceiling 3.4) 4.0)
          (== (ceiling -3.4) -3.0)
          (== (ceiling 3) 3.0))

(describe sign
          (== (sign 0) 0)
          (== (sign 0.0) 0)
          (== (sign 5) 1)
          (== (sign 5.5) 1)
          (== (sign -5) -1)
          (== (sign -5.5) -1))
