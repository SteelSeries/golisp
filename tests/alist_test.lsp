(describe alist
          (== (alist 42) 42))

(describe acons
          (== (acons 'a 1 '()) (alist '((a . 1))))
          (== (acons 'a 1 (alist '((b . 2) (c . 3))))
              (alist '((a . 1) (b . 2) (c . 3))))
          (== (acons 'a 5 (alist '((a . 1) (b . 2) (c . 3))))
              (alist '((a . 5) (b . 2) (c . 3)))))

(describe pairlis
          (== (pairlis '(a b) '(1 2)) (alist '((b . 2) (a . 1))))
          (== (pairlis '(a b) '(1 2) '((c . 3) (d . 4))) (alist '((b . 2) (a . 1) (c . 3) (d . 4)))))

(describe assoc
          (== (assoc 'a (alist '((a . 1) (b . 2) (c . 3)))) '(a . 1))
          (== (assoc 'b (alist '((a . 1) (b . 2) (c . 3)))) '(b . 2))
          (== (assoc 'c (alist '((a . 1) (b . 2)))) '()))

(describe rassoc
          (== (rassoc 1 (alist '((a . 1) (b . 2) (c . 3)))) '(a . 1))
          (== (rassoc 2 (alist '((a . 1) (b . 2) (c . 3)))) '(b . 2))
          (== (rassoc 3 (alist '((a . 1) (b . 2)))) '()))

(describe dissoc
          (== (dissoc 'a (alist '((a . 1) (b . 2) (c . 3)))) (alist '((b . 2) (c . 3)))))
