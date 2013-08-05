(describe arithmetic
    (== (+ 5.0 5.0) 10.0)
    (== (- 10.0 7.0) 3.0)
    (== (- 7.0 10.0) -3.0)
    (== (* 2.0 4.0)  8.0)
    (== (/ 25.0 5.0) 5.0))

(define fx 2.2)
(define fy 8.8)
(define fz 7.7)

(describe conditions
    (== (< fx fy)  #t)
    (== (< fy fz)  #f)
    (== (> fx fy)  #f)
    (== (> fz fx)  #t)
    (== (<= fx 2.2) #t)
    (== (>= fz 7) #t))

(describe conversions
          (== (float 5) 5.0)
          (== (integer 5.6) 5))

