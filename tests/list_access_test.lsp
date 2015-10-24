;;; -*- mode: Scheme -*-

// Test list access primitives

(define l '(1 2 3 4 5 6 7 8 9 10))

(context "list access"

         ()
         
         (it "list"
             (assert-eq (list 'a) '(a))
             (assert-eq (list (+ 1 1) (+ 1 2)) '(2 3)))

         (it "length"
             (assert-eq (length nil) 0)
             (assert-eq (length '()) 0)
             (assert-eq (length '(1)) 1)
             (assert-eq (length '(1 2)) 2)
             (assert-eq (length l) 10))

         (it "first"
             (assert-eq (first 'a) nil)
             (assert-eq (first nil) nil)
             (assert-eq (first '()) nil)
             (assert-eq (first '(1)) 1)
             (assert-eq (first l) 1))

         (it "second"
             (assert-eq (second 'a) nil)
             (assert-eq (second nil) nil)
             (assert-eq (second '()) nil)
             (assert-eq (second '(1 2)) 2)
             (assert-eq (second l) 2))

         (it "third"
             (assert-eq (third 'a) nil)
             (assert-eq (third nil) nil)
             (assert-eq (third '()) nil)
             (assert-eq (third '(1 2 3)) 3)
             (assert-eq (third l) 3))

         (it "fourth"
             (assert-eq (fourth 'a) nil)
             (assert-eq (fourth nil) nil)
             (assert-eq (fourth '()) nil)
             (assert-eq (fourth '(1 2 3 4)) 4)
             (assert-eq (fourth l) 4))

         (it "fifth"
             (assert-eq (fifth 'a) nil)
             (assert-eq (fifth nil) nil)
             (assert-eq (fifth '()) nil)
             (assert-eq (fifth '(1 2 3 4 5)) 5)
             (assert-eq (fifth l) 5))

         (it "sixth"
             (assert-eq (sixth 'a) nil)
             (assert-eq (sixth nil) nil)
             (assert-eq (sixth '()) nil)
             (assert-eq (sixth '(1 2 3 4 5 6)) 6)
             (assert-eq (sixth l) 6))

         (it "seventh"
             (assert-eq (seventh 'a) nil)
             (assert-eq (seventh nil) nil)
             (assert-eq (seventh '()) nil)
             (assert-eq (seventh '(1 2 3 4 5 6 7)) 7)
             (assert-eq (seventh l) 7))

         (it "eighth"
             (assert-eq (eighth 'a) nil)
             (assert-eq (eighth nil) nil)
             (assert-eq (eighth '()) nil)
             (assert-eq (eighth '(1 2 3 4 5 6 7 8)) 8)
             (assert-eq (eighth l) 8))

         (it "ninth"
             (assert-eq (ninth 'a) nil)
             (assert-eq (ninth nil) nil)
             (assert-eq (ninth '()) nil)
             (assert-eq (ninth '(1 2 3 4 5 6 7 8 9)) 9)
             (assert-eq (ninth l) 9))

         (it "tenth"
             (assert-eq (tenth 'a) nil)
             (assert-eq (tenth nil) nil)
             (assert-eq (tenth '()) nil)
             (assert-eq (tenth '(1 2 3 4 5 6 7 8 9 10)) 10)
             (assert-eq (tenth l) 10))

         (it "nth"
             (assert-eq (nth 0 nil) nil)
             (assert-eq (nth 0 '()) nil)
             (assert-eq (nth 0 l) 1)
             (assert-eq (nth 1 l) 2)
             (assert-eq (nth 2 l) 3)
             (assert-eq (nth 3 l) 4)
             (assert-eq (nth 4 l) 5)
             (assert-eq (nth 5 l) 6)
             (assert-eq (nth 6 l) 7)
             (assert-eq (nth 7 l) 8)
             (assert-eq (nth 8 l) 9)
             (assert-eq (nth 9 l) 10)
             (assert-error (nth -1 l))  ;1st arg can't be negative
             (assert-error (nth 10 l)) ;1st arg must be in range
             (assert-error (nth 'a '(1)))  ;1st arg must be a number
             (assert-error (nth 1 2)))    ;2nd arg must be a list


         (it "list-ref"
             (assert-eq (list-ref nil 1) nil)
             (assert-eq (list-ref '() 1) nil)
             (assert-eq (list-ref l 0) 1)
             (assert-eq (list-ref l 1) 2)
             (assert-eq (list-ref l 2) 3)
             (assert-eq (list-ref l 3) 4)
             (assert-eq (list-ref l 4) 5)
             (assert-eq (list-ref l 5) 6)
             (assert-eq (list-ref l 6) 7)
             (assert-eq (list-ref l 7) 8)
             (assert-eq (list-ref l 8) 9)
             (assert-eq (list-ref l 9) 10)
             (assert-error (list-ref l 10))
             (assert-error (list-ref 5 1))      ;1st arg must be a list
             (assert-error (list-ref '(1) 'a))) ;2nd arg must be a number

         (it "car"
             (assert-eq (car 'a) nil)
             (assert-eq (car nil) nil)
             (assert-eq (car '(1)) 1)
             (assert-eq (car l) 1))

         (it "cdr"
             (assert-eq (cdr 'a) nil)
             (assert-eq (cdr nil) nil)
             (assert-eq (cdr '(1)) nil)
             (assert-eq (length (cdr l)) 9))

         (it "caar"
             (assert-eq (caar 'a) nil)
             (assert-eq (caar nil) nil)
             (assert-eq (caar '((1))) 1))

         (it "cadr"
             (assert-eq (cadr 'a) nil)
             (assert-eq (cadr nil) nil)
             (assert-eq (cadr '(1 2)) 2))

         (it "cdar"
             (assert-eq (cdar 'a) nil)
             (assert-eq (cdar nil) nil)
             (assert-eq (cdar '(1)) nil)
             (assert-eq (cdar '((1 2) 3)) '(2)))

         (it "cddr"
             (assert-eq (cddr 'a) nil)
             (assert-eq (cddr nil) nil)
             (assert-eq (cddr '(1)) nil)
             (assert-eq (cddr '(1 2 3)) '(3)))

         (it "caaar"
             (assert-eq (caaar 'a) nil)
             (assert-eq (caaar nil) nil)
             (assert-eq (caaar '(((1)))) 1))

         (it "caadr"
             (assert-eq (caadr 'a) nil)
             (assert-eq (caadr nil) nil)
             (assert-eq (caadr '(1 (2))) 2))

         (it "cadar"
             (assert-eq (cadar 'a) nil)
             (assert-eq (cadar nil) nil)
             (assert-eq (cadar '(1)) nil)
             (assert-eq (cadar '((1 2) 3)) 2))

         (it "caddr"
             (assert-eq (caddr 'a) nil)
             (assert-eq (caddr nil) nil)
             (assert-eq (caddr '(1)) nil)
             (assert-eq (caddr '(1 2 3)) 3))

         (it "cdaar"
             (assert-eq (cdaar 'a) nil)
             (assert-eq (cdaar nil) nil)
             (assert-eq (cdaar '(((1 2)))) '(2)))

         (it "cdadr"
             (assert-eq (cdadr 'a) nil)
             (assert-eq (cdadr nil) nil)
             (assert-eq (cdadr '(1 (1 2))) '(2)))

         (it "cddar"
             (assert-nil (cddar 'a))
             (assert-nil (cddar nil))
             (assert-nil (cddar '(1)))
             (assert-eq (cddar '((1 2 4) 3))
                        '(4)))

         (it "cdddr"
             (assert-nil (cdddr 'a))
             (assert-nil (cdddr nil))
             (assert-nil (cdddr '(1)))
             (assert-eq (cdddr '(1 2 3 4))
                        '(4)))

         (it "general-car-cdr"
             (assert-eq (general-car-cdr '(1 2 3 4) #b1100)
                        3)
             (assert-eq (general-car-cdr '(1 2 (3 4)) #b1100)
                        '(3 4))
             (assert-eq (general-car-cdr '(1 2 (3 4)) #b110100)
                        4)
             (assert-error (general-car-cdr '(1 2 3) 0))) ;needs a non-zero path specifier

         (it "last-pair"
             (assert-eq (last-pair '(1 2 3))
                        '(3))
             (assert-eq (last-pair '(1 2 . 3))
                        '(2 . 3))
             (assert-error (last-pair '())) ;needs non-empty list
             (assert-error (last-pair 5))) ;needs a list

         )
