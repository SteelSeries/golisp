// Test list access primitives

(define l '(1 2 3 4 5 6 7 8 9 10))

(describe list
          (== (list 'a) '(a))
          (== (list (+ 1 1) (+ 1 2)) '(2 3)))

(describe length
          (== (length nil) 0)
          (== (length '()) 0)
          (== (length '(1)) 1)
          (== (length '(1 2)) 2)
          (== (length l) 10))

(describe first
          (== (first 'a) nil)
          (== (first nil) nil)
          (== (first '()) nil)
          (== (first '(1)) 1)
          (== (first l) 1))

(describe second
          (== (second 'a) nil)
          (== (second nil) nil)
          (== (second '()) nil)
          (== (second '(1 2)) 2)
          (== (second l) 2))

(describe third
          (== (third 'a) nil)
          (== (third nil) nil)
          (== (third '()) nil)
          (== (third '(1 2 3)) 3)
          (== (third l) 3))

(describe fourth
          (== (fourth 'a) nil)
          (== (fourth nil) nil)
          (== (fourth '()) nil)
          (== (fourth '(1 2 3 4)) 4)
          (== (fourth l) 4))

(describe fifth
          (== (fifth 'a) nil)
          (== (fifth nil) nil)
          (== (fifth '()) nil)
          (== (fifth '(1 2 3 4 5)) 5)
          (== (fifth l) 5))

(describe nth
          (== (nth nil 1) nil)
          (== (nth '() 1) nil)
          (== (nth l 0) nil)
          (== (nth l 1) 1)
          (== (nth l 2) 2)
          (== (nth l 3) 3)
          (== (nth l 4) 4)
          (== (nth l 5) 5)
          (== (nth l 6) 6)
          (== (nth l 7) 7)
          (== (nth l 8) 8)
          (== (nth l 9) 9)
          (== (nth l 10) 10)
          (== (nth l 11) nil))

(describe car
          (== (car 'a) nil)
          (== (car nil) nil)
          (== (car '(1)) 1)
          (== (car l) 1))

(describe cdr
          (== (cdr 'a) nil)
          (== (cdr nil) nil)
          (== (cdr '(1)) nil)
          (== (length (cdr l)) 9))

(describe caar
          (== (caar 'a) nil)
          (== (caar nil) nil)
          (== (caar '((1))) 1))

(describe cadr
          (== (cadr 'a) nil)
          (== (cadr nil) nil)
          (== (cadr '(1 2)) 2))

(describe cdar
          (== (cdar 'a) nil)
          (== (cdar nil) nil)
          (== (cdar '(1)) nil)
          (== (cdar '((1 2) 3)) '(2)))

(describe cddr
          (== (cddr 'a) nil)
          (== (cddr nil) nil)
          (== (cddr '(1)) nil)
          (== (cddr '(1 2 3)) '(3)))


