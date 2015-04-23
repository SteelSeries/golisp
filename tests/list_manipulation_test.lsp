(describe cons
          (== (cons 'a 'b) '(a . b))
          (== (cons 'a '(b c)) '(a b c)))

(describe reverse
          (== (reverse '(a)) '(a))
          (== (reverse '(a b)) '(b a))
          (== (reverse '(a b c d)) '(d c b a))
          (== (reverse (list)) '())
          (== (reverse 42) 42))

(describe flatten
          (== (flatten '(1 2 3 4)) '(1 2 3 4))
          (== (flatten '(1 (2 3) 4)) '(1 2 3 4))
          (== (flatten '(1 (2 (3 4) 5) 6)) '(1 2 (3 4) 5 6))
          (== (flatten (list)) '())
          (== (flatten 42) 42))

(describe flatten*
          (== (flatten* '(1 2 3 4)) '(1 2 3 4))
          (== (flatten* '(1 (2 3) 4)) '(1 2 3 4))
          (== (flatten* '(1 (2 (3 4) 5) 6)) '(1 2 3 4 5 6))
          (== (flatten* '(1 (2 (3 (7 8) 4) 5) 6)) '(1 2 3 7 8 4 5 6))
          (== (flatten* (list)) '())
          (== (flatten* 42) 42))

(describe partition
          (== (partition 2 '(1 2 3 4 5 6 7 8)) '((1 2) (3 4) (5 6) (7 8)))
          (== (partition 4 '(1 2 3 4 5 6 7 8)) '((1 2 3 4) (5 6 7 8))))

(define list1 '(1 2))
(describe append
          (== (append list1 '(3 4)) '(1 2 3 4))
          (== list1 '(1 2))
          (== (append list1 42) '(1 2 42))
          (== list1 '(1 2))
          (== (append '() 42) '(42))
          (== (append '() '(1 2)) '(1 2)))

(define list1 '(1 2))
(define list2 '(1 2))
(define list3 (list))
(describe append!
          (== (append! list1 '(3 4)) '(1 2 3 4))
          (== list1 '(1 2 3 4))
          (== (append! list2 42) '(1 2 42))
          (== list2 '(1 2 42))
          (== (append! '() 42) '(42))
          (== (append! '() '(1 2)) '(1 2))
          (== (append! list3 42) '(42))
          (== list3 '(42)))

(describe take
          (== (take 0 '(1 2 3)) '())
          (== (take 1 '(1 2 3)) '(1))
          (== (take 3 '(1 2 3)) '(1 2 3))
          (== (take 3 '(1 2 3 4 5)) '(1 2 3)))

(describe drop
          (== (drop 0 '(1 2 3)) '(1 2 3))
          (== (drop 1 '(1 2 3)) '(2 3))
          (== (drop 3 '(1 2 3)) '())
          (== (drop 3 '(1 2 3 4 5)) '(4 5)))

(describe list-head
          (== (list-head '(1 2 3) 0) '())
          (== (list-head '(1 2 3) 1) '(1))
          (== (list-head '(1 2 3) 3) '(1 2 3))
          (== (list-head '(1 2 3 4 5) 3) '(1 2 3)))
    
(describe list-tail
          (== (list-tail '(1 2 3) 0) '(1 2 3))
          (== (list-tail '(1 2 3) 1) '(2 3))
          (== (list-tail '(1 2 3) 3) '())
          (== (list-tail '(1 2 3 4 5) 3) '(4 5)))

(describe sublist
          (== (sublist 1 3 '(1 2 3)) '(1 2 3))
          (== (sublist 2 4 '(1 2 3 4 5)) '(2 3 4))
          (== (sublist 2 2 '(1 2 3 4 5)) '()))

(describe make-list
          (== (make-list 5) '(() () () () ()))
          (== (make-list 5 1) '(1 1 1 1 1))
          (== (make-list 3 'a) '(a a a)))
