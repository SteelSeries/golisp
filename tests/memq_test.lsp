(describe memq-with-simple-list
          (== (memq 'a '(a b c)) '(a b c))
          (== (memq 'b '(a b c)) '(b c))
          (== (memq 'c '(a b c)) '(c))
          (== (memq 'd '(a b c)) #f))

(describe memq-with-list-of-numbers
          (== (memq 1 '(1 2 3)) '(1 2 3))
          (== (memq 2 '(1 2 3)) '(2 3))
          (== (memq 4 '(1 2 3)) #f))

(describe find
          (== (find even? '(3 1 4 1 5 9)) 4)
          (== (find even? '(1 3 5 7 9)) #f))

(describe find-tail
          (== (find-tail even? '(3 1 4 1 5 9)) '(4 1 5 9))
          (== (find-tail even? '(1 3 5 7 9)) #f))

(describe memp
          (== (memp even? '(3 1 4 1 5 9)) '(4 1 5 9))
          (== (memp even? '(1 3 5 7 9)) #f))
