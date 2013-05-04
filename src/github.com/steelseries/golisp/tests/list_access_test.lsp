


(describe "car"
          (== (car nil) nil)
          (== (car '(1)') 1)
          (== (car '(1 2)') 1))

(describe failure
          (== (car '(1 3)') 2))
