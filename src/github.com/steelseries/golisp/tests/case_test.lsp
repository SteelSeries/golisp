(define (test-func x)
  (case x
    (0 "zero")
    (1 "one")
    (2 "two")
    (3 "three")
    "unknown"))

(describe case
          (== (test-func 0) "zero")
          (== (test-func 1) "one")
          (== (test-func 2) "two")
          (== (test-func 3) "three")
          (== (test-func 5) "unknown"))
