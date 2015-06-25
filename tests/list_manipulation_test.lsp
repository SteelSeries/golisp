;;; -*- mode: Scheme -*-

(describe cons
          (assert-eq (cons 'a 'b)
                     '(a . b))
          (assert-eq (cons 'a '(b c))
                     '(a b c)))

(describe reverse
          (assert-eq (reverse '(a))
                     '(a))
          (assert-eq (reverse '(a b))
                     '(b a))
          (assert-eq (reverse '(a b c d))
                     '(d c b a))
          (assert-eq (reverse (list))
                     '())
          (assert-eq (reverse 42)
                     42))

(describe flatten
          (assert-eq (flatten '(1 2 3 4))
                     '(1 2 3 4))
          (assert-eq (flatten '(1 (2 3) 4))
                     '(1 2 3 4))
          (assert-eq (flatten '(1 (2 (3 4) 5) 6))
                     '(1 2 (3 4) 5 6))
          (assert-eq (flatten (list))
                     '())
          (assert-eq (flatten 42)
                     42))

(describe flatten*
          (assert-eq (flatten* '(1 2 3 4))
                     '(1 2 3 4))
          (assert-eq (flatten* '(1 (2 3) 4))
                     '(1 2 3 4))
          (assert-eq (flatten* '(1 (2 (3 4) 5) 6))
                     '(1 2 3 4 5 6))
          (assert-eq (flatten* '(1 (2 (3 (7 8) 4) 5) 6))
                     '(1 2 3 7 8 4 5 6))
          (assert-eq (flatten* (list))
                     '())
          (assert-eq (flatten* 42)
                     42))

(describe partition-by-size
          (assert-eq (partition 2 '(1 2 3 4 5 6 7 8))
                     '((1 2) (3 4) (5 6) (7 8)))
          (assert-eq (partition 4 '(1 2 3 4 5 6 7 8))
                     '((1 2 3 4) (5 6 7 8))))

(describe partition-by-predicate
          (assert-eq (partition odd? '(1 2 3 4 5 6 7 8 9))
                     '((1 3 5 7 9) (2 4 6 8)))
          (assert-eq (partition even? '(1 2 3 4 5 6 7 8 9))
                     '((2 4 6 8) (1 3 5 7 9))))

(define list1 '(1 2))
(describe append
          (assert-eq (append list1 '(3 4))
                     '(1 2 3 4))
          (assert-eq list1
                     '(1 2))
          (assert-eq (append list1 42)
                     '(1 2 42))
          (assert-eq list1
                     '(1 2))
          (assert-eq (append '() 42)
                     '(42))
          (assert-eq (append '() '(1 2))
                     '(1 2)))

(define list1 '(1 2))
(define list2 '(1 2))
(define list3 (list))
(describe append!
          (assert-eq (append! list1 '(3 4))
                     '(1 2 3 4))
          (assert-eq list1
                     '(1 2 3 4))
          (assert-eq (append! list2 42)
                     '(1 2 42))
          (assert-eq list2
                     '(1 2 42))
          (assert-eq (append! '() 42)
                     '(42))
          (assert-eq (append! '() '(1 2))
                     '(1 2))
          (assert-eq (append! list3 42)
                     '(42))
          (assert-eq list3
                     '(42)))

(describe take
          (assert-eq (take 0 '(1 2 3))
                     '())
          (assert-eq (take 1 '(1 2 3))
                     '(1))
          (assert-eq (take 3 '(1 2 3))
                     '(1 2 3))
          (assert-eq (take 3 '(1 2 3 4 5))
                     '(1 2 3)))

(describe drop
          (assert-eq (drop 0 '(1 2 3))
                     '(1 2 3))
          (assert-eq (drop 1 '(1 2 3))
                     '(2 3))
          (assert-eq (drop 3 '(1 2 3))
                     '())
          (assert-eq (drop 3 '(1 2 3 4 5))
                     '(4 5)))

(describe list-head
          (assert-eq (list-head '(1 2 3) 0)
                     '())
          (assert-eq (list-head '(1 2 3) 1)
                     '(1))
          (assert-eq (list-head '(1 2 3) 3)
                     '(1 2 3))
          (assert-eq (list-head '(1 2 3 4 5) 3)
                     '(1 2 3)))

(describe list-tail
          (assert-eq (list-tail '(1 2 3) 0)
                     '(1 2 3))
          (assert-eq (list-tail '(1 2 3) 1)
                     '(2 3))
          (assert-eq (list-tail '(1 2 3) 3)
                     '())
          (assert-eq (list-tail '(1 2 3 4 5) 3)
                     '(4 5)))

(describe sublist
          (assert-eq (sublist '(1 2 3) 1 3)
                     '(1 2))
          (assert-eq (sublist '(1 2 3 4 5) 2 4)
                     '(2 3))
          (assert-eq (sublist '(1 2 3 4 5) 2 2)
                     '()))

(describe make-list
          (assert-eq (make-list 5)
                     '(() () () () ()))
          (assert-eq (make-list 5 1)
                     '(1 1 1 1 1))
          (assert-eq (make-list 3 'a)
                     '(a a a)))
