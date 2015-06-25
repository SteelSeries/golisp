;;; -*- mode: Scheme -*-

(describe alist
          (assert-eq (alist 42) 42))

(describe acons
          (assert-eq (acons 'a 1 '())
                     (alist '((a . 1))))
          (assert-eq (acons 'a 1 (alist '((b . 2) (c . 3))))
                     (alist '((a . 1) (b . 2) (c . 3))))
          (assert-eq (acons 'a 5 (alist '((a . 1) (b . 2) (c . 3))))
                     (alist '((a . 5) (b . 2) (c . 3)))))

(describe pairlis
          (assert-eq (pairlis '(a b) '(1 2))
                     (alist '((b . 2) (a . 1))))
          (assert-eq (pairlis '(a b) '(1 2) '((c . 3) (d . 4)))
                     (alist '((b . 2) (a . 1) (c . 3) (d . 4)))))

(describe assoc
          (assert-eq (assoc 'a (alist '((a . 1) (b . 2) (c . 3))))
                     '(a . 1))
          (assert-eq (assoc 'b (alist '((a . 1) (b . 2) (c . 3))))
                     '(b . 2))
          (assert-eq (assoc 'c (alist '((a . 1) (b . 2))))
                     '()))

(describe rassoc
          (assert-eq (rassoc 1 (alist '((a . 1) (b . 2) (c . 3))))
                     '(a . 1))
          (assert-eq (rassoc 2 (alist '((a . 1) (b . 2) (c . 3))))
                     '(b . 2))
          (assert-eq (rassoc 3 (alist '((a . 1) (b . 2))))
                     '()))

(describe dissoc
          (assert-eq (dissoc 'a (alist '((a . 1) (b . 2) (c . 3))))
                     (alist '((b . 2) (c . 3)))))
