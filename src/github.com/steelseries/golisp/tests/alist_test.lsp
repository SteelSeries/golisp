(describe acons
          (== (acons 'a 1 '()) '((a.1)))
          (== (acons 'a 1 '((b.2) (c.3)))
              '((a.1) (b.2) (c.3))))

(describe pairlis
          (== (pairlis '(a b) '(1 2)) '((b.2) (a.1)))
          (== (pairlis '(a b) '(1 2) '((c.3) (d.4))) '((b.2) (a.1) (c.3) (d.4))))

(describe assoc
          (== (assoc 'a '((a.1) (b.2) (c.3))) '(a.1))
          (== (assoc 'b '((a.1) (b.2) (c.3))) '(b.2)))

(describe rassoc
          (== (rassoc '1 '((a.1) (b.2) (c.3))) '(a.1))
          (== (rassoc '2 '((a.1) (b.2) (c.3))) '(b.2)))
