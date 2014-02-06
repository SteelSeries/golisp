(describe cons
          (== (cons 'a 'b) '(a . b))
          (== (cons 'a '(b c)) '(a b c)))

(describe reverse
          (== (reverse '(a)) '(a))
          (== (reverse '(a b)) '(b a))
          (== (reverse '(a b c d)) '(d c b a)))

(describe flatten
          (== (flatten '(1 2 3 4)) '(1 2 3 4))
          (== (flatten '(1 (2 3) 4)) '(1 2 3 4))
          (== (flatten '(1 (2 (3 4) 5) 6)) '(1 2 (3 4) 5 6)))

(describe flatten*
          (== (flatten* '(1 2 3 4)) '(1 2 3 4))
          (== (flatten* '(1 (2 3) 4)) '(1 2 3 4))
          (== (flatten* '(1 (2 (3 4) 5) 6)) '(1 2 3 4 5 6))
          (== (flatten* '(1 (2 (3 (7 8) 4) 5) 6)) '(1 2 3 7 8 4 5 6)))

(describe partition
          (== (partition 2 '(1 2 3 4 5 6 7 8)) '((1 2) (3 4) (5 6) (7 8)))
          (== (partition 4 '(1 2 3 4 5 6 7 8)) '((1 2 3 4) (5 6 7 8))))

(describe append
          (== (append '(1 2) '(3 4)) '(1 2 3 4)))

(describe append!
          (define list1 '(1 2))
          (== (append! list1 '(3 4)) '(1 2 3 4))
          (== list1 '(1 2 3 4)))

 (describe take
    (== (take 1 '(1 2 3)) '(1))
    (== (take 3 '(1 2 3)) '(1 2 3))
    (== (take 3 '(1 2 3 4 5)) '(1 2 3)))