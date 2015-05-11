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

(describe sixth
          (== (sixth 'a) nil)
          (== (sixth nil) nil)
          (== (sixth '()) nil)
          (== (sixth '(1 2 3 4 5 6)) 6)
          (== (sixth l) 6))

(describe seventh
          (== (seventh 'a) nil)
          (== (seventh nil) nil)
          (== (seventh '()) nil)
          (== (seventh '(1 2 3 4 5 6 7)) 7)
          (== (seventh l) 7))

(describe eighth
          (== (eighth 'a) nil)
          (== (eighth nil) nil)
          (== (eighth '()) nil)
          (== (eighth '(1 2 3 4 5 6 7 8)) 8)
          (== (eighth l) 8))

(describe ninth
          (== (ninth 'a) nil)
          (== (ninth nil) nil)
          (== (ninth '()) nil)
          (== (ninth '(1 2 3 4 5 6 7 8 9)) 9)
          (== (ninth l) 9))

(describe tenth
          (== (tenth 'a) nil)
          (== (tenth nil) nil)
          (== (tenth '()) nil)
          (== (tenth '(1 2 3 4 5 6 7 8 9 10)) 10)
          (== (tenth l) 10))

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

(describe caaar
          (== (caaar 'a) nil)
          (== (caaar nil) nil)
          (== (caaar '(((1)))) 1))

(describe caadr
          (== (caadr 'a) nil)
          (== (caadr nil) nil)
          (== (caadr '(1 (2))) 2))

(describe cadar
          (== (cadar 'a) nil)
          (== (cadar nil) nil)
          (== (cadar '(1)) nil)
          (== (cadar '((1 2) 3)) 2))

(describe caddr
          (== (caddr 'a) nil)
          (== (caddr nil) nil)
          (== (caddr '(1)) nil)
          (== (caddr '(1 2 3)) 3))

(describe cdaar
          (== (cdaar 'a) nil)
          (== (cdaar nil) nil)
          (== (cdaar '(((1 2)))) '(2)))

(describe cdadr
          (== (cdadr 'a) nil)
          (== (cdadr nil) nil)
          (== (cdadr '(1 (1 2))) '(2)))

(describe cddar
          (== (cddar 'a) nil)
          (== (cddar nil) nil)
          (== (cddar '(1)) nil)
          (== (cddar '((1 2 4) 3)) '(4)))

(describe cdddr
          (== (cdddr 'a) nil)
          (== (cdddr nil) nil)
          (== (cdddr '(1)) nil)
          (== (cdddr '(1 2 3 4)) '(4)))

(describe last-pair
          (== (last-pair '(1 2 3)) '(3))
          (== (last-pair '(1 2 . 3)) '(2 . 3)))
