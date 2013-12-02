(describe chain
          (== (-> 1) 1)
          (== (-> 1 (+ 2)) 3)
          (== (-> 1 (+ 2) (* 3)) 9)
          (== (-> 1 (+ 2) str) "3"))

(describe tap
          (== (tap 1 (* 2) str) 1)
          (== (let ((a 1)
                    (foo (lambda (x) (set! a x))))
                (tap 4 foo)
                a)
              4))
