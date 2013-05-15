(describe cons
          (== (cons 'a 'b) '(a . b))
          (== (cons 'a '(b c)) '(a b c)))

(describe reverse
          (== (reverse '(a)) '(a))
          (== (reverse '(a b)) '(b a))
          (== (reverse '(a b c d)) '(d c b a)))
