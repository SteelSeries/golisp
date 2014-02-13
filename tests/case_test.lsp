(define (test-func x)
  (case x
    ((0) "zero")
    ((1) "one")
    ((2) "two")
    ((3) "three")
    (else "unknown")))

(describe case
          (== (test-func 0) "zero")
          (== (test-func 1) "one")
          (== (test-func 2) "two")
          (== (test-func 3) "three")
          (== (test-func 5) "unknown"))

(define (complex-func x)
  (let ((y 1))
    (case x
      ((0) (set! y 2)
         (+ y 1))
      ((1) (set! y 5)
         (+ y 2))
      (else (set! y 10)
            (+ y 16)))))

(describe complex-case
          (== (complex-func 0) 3)
          (== (complex-func 1) 7)
          (== (complex-func 42) 26))

(define (multi-test-func x)
    (case x
        ((0) "none")
        ((1) "one")
        ((2) "a couple")
        ((3 4 5) "a few")
        ((6 7 8) "some")
        (else "many")))
        
(describe multi-case
    (== (multi-test-func 0) "none")
    (== (multi-test-func 1) "one")
    (== (multi-test-func 2) "a couple")
    (== (multi-test-func 3) "a few")
    (== (multi-test-func 4) "a few")
    (== (multi-test-func 5) "a few")
    (== (multi-test-func 6) "some")
    (== (multi-test-func 7) "some")
    (== (multi-test-func 8) "some")
    (== (multi-test-func 9) "many"))
    