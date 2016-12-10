;;; -*- mode: Scheme -*-

(define list1 '(1 2))
(define list1a '(1 2))
(define list2 '(1 2))
(define list3 (list))

(context "list manipulation"

         ()

         (it "cons"
             (assert-eq (cons 'a 'b) '(a . b))
             (assert-eq (cons 'a '(b c)) '(a b c)))

         (it "cons*"
             (assert-eq (cons* 'a 'b 'c) '(a b . c))
             (assert-eq (cons* 'a 'b '(c d)) '(a b c d))
             (assert-eq (cons* 'a) 'a)
             (assert-eq (cons* '(a b)) '(a b)))

         (it "reverse"
             (assert-eq (reverse '(a)) '(a))
             (assert-eq (reverse '(a b)) '(b a))
             (assert-eq (reverse '(a b c d)) '(d c b a))
             (assert-eq (reverse (list)) '())
             (assert-error (reverse 42)))
         
         (it "flatten"
             (assert-eq (flatten '(1 2 3 4)) '(1 2 3 4))
             (assert-eq (flatten '(1 (2 3) 4)) '(1 2 3 4))
             (assert-eq (flatten '(1 (2 (3 4) 5) 6)) '(1 2 (3 4) 5 6))
             (assert-eq (flatten (list)) '())
             (assert-error (flatten 42)))

         (it "flatten*"
             (assert-eq (flatten* '(1 2 3 4)) '(1 2 3 4))
             (assert-eq (flatten* '(1 (2 3) 4)) '(1 2 3 4))
             (assert-eq (flatten* '(1 (2 (3 4) 5) 6)) '(1 2 3 4 5 6))
             (assert-eq (flatten* '(1 (2 (3 (7 8) 4) 5) 6)) '(1 2 3 7 8 4 5 6))
             (assert-eq (flatten* (list)) '())
             (assert-error (flatten* 42)))

         (it "partition-by-size"
             (assert-eq (partition 2 '(1 2 3 4 5 6 7 8)) '((1 2) (3 4) (5 6) (7 8)))
             (assert-eq (partition 4 '(1 2 3 4 5 6 7 8)) '((1 2 3 4) (5 6 7 8))))

         (it "partition-by-predicate"
             (assert-eq (partition odd? '(1 2 3 4 5 6 7 8 9)) '((1 3 5 7 9) (2 4 6 8)))
             (assert-eq (partition even? '(1 2 3 4 5 6 7 8 9)) '((2 4 6 8) (1 3 5 7 9))))

         (it "partition-errors"
             (assert-error (partition -1 '(1 2))) ;1st arg has to be non -ive if it's an int
             (assert-error (partition "hi" '(1 2)))  ;1st arg has to be int or function
             (assert-error (partition 1 "1 2")) ;2nd arg must be a list
             (assert-error (partition odd? "1 2"))) ;2nd arg must be a list

         (it "append"
             (assert-eq (append '(x) '(y)) '(x y))
             (assert-eq (append '(a) '(b c d)) '(a b c d))
             (assert-eq (append '(a (b)) '((c))) '(a (b) (c)))
             (assert-eq (append) '())
             (assert-eq (append '(a b) '(c . d)) '(a b c . d))
             (assert-eq (append '() 'a) 'a))

         (it "take"
             (assert-eq (take 0 '(1 2 3)) '())
             (assert-eq (take 1 '(1 2 3)) '(1))
             (assert-eq (take 3 '(1 2 3)) '(1 2 3))
             (assert-eq (take 3 '(1 2 3 4 5)) '(1 2 3))
             (assert-error (take "1" '(1 2 3))) ;1st arg must be a number
             (assert-error (take 1 4))) ;2nd arg must be a list

         (it "drop"
             (assert-eq (drop 0 '(1 2 3)) '(1 2 3))
             (assert-eq (drop 1 '(1 2 3)) '(2 3))
             (assert-eq (drop 3 '(1 2 3)) '())
             (assert-eq (drop 3 '(1 2 3 4 5)) '(4 5))
             (assert-error (drop "1" '(1 2 3))) ;1st arg must be a number
             (assert-error (drop 1 4))) ;2nd arg must be a list

         (it "list-head"
             (assert-eq (list-head '(1 2 3) 0) '())
             (assert-eq (list-head '(1 2 3) 1) '(1))
             (assert-eq (list-head '(1 2 3) 3) '(1 2 3))
             (assert-eq (list-head '(1 2 3 4 5) 3) '(1 2 3))
             (assert-error (list-head 4 5)) ;1st arg must be a list
             (assert-error (list-head '(1 2 3) "6"))) ;2nd arg must be a number

         (it "list-tail"
             (assert-eq (list-tail '(1 2 3) 0) '(1 2 3))
             (assert-eq (list-tail '(1 2 3) 1) '(2 3))
             (assert-eq (list-tail '(1 2 3) 3) '())
             (assert-eq (list-tail '(1 2 3 4 5) 3) '(4 5))
             (assert-error (list-tail 4 5)) ;1st arg must be a list
             (assert-error (list-tail '(1 2 3) "6"))) ;2nd arg must be a number

         (it "sublist"
             (assert-eq (sublist '(1 2 3) 1 3) '(2 3))
             (assert-eq (sublist '(1 2 3 4 5) 2 4) '(3 4))
             (assert-eq (sublist '(1 2 3 4 5) 2 2) '())
             (assert-eq (sublist '(1 2 3 4 5) 2 3) '(3))
             (assert-error (sublist 1 2 3)) ;1st arg must be a list
             (assert-error (sublist '(1) "1" 2)) ;2nd arg must be an int
             (assert-error (sublist '(1) -1 2)) ;2nd arg must be an +ive int
             (assert-error (sublist '(1) 1 "2")) ;3rd arg must be an int
             (assert-error (sublist '(1) 1 -2)) ;3rd arg must be an +ive int
             )

         (it "make-list"
             (assert-eq (make-list 5) '(() () () () ()))
             (assert-eq (make-list 5 1) '(1 1 1 1 1))
             (assert-eq (make-list 3 'a) '(a a a))
             (assert-error (make-list "3" 1)) ;1st arg must be an integer
             (assert-error (make-list 3.4 1)) ;1st arg must be an integer
             (assert-error (make-list -3 1))) ;1st arg must be a non-negative integer
         )

(context "Appending in place"
         
         ((define x '(a b c))
          (define y '(d e f))
          (define z '(g h))
          (define a (append! x y z)))
         
         (it "combines args"
             (assert-eq a '(a b c d e f g h))
             (assert-eq x '(a b c d e f g h))
             (assert-eq y '(d e f g h))
             (assert-eq z '(g h)))
         
         )
