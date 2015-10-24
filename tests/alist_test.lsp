;;; -*- mode: Scheme -*-

(context "alist"

         ()
         
         (it "can cons"
                   (assert-eq (acons 'a 1 '())
                              '((a . 1)))
                   (assert-eq (acons 'a 1 '((b . 2) (c . 3)))
                              '((a . 1) (b . 2) (c . 3)))
                   (assert-eq (acons 'a 5 '((a . 1) (b . 2) (c . 3)))
                              '((a . 5) (b . 2) (c . 3)))

                   (assert-error (acons '(1 2) 1 '()))) ;first arg can not be a list

         (it "can combine lists"
                   (assert-eq (pairlis '(a b) '(1 2))
                              '((b . 2) (a . 1)))
                   (assert-eq (pairlis '(a b) '(1 2) '((c . 3) (d . 4)))
                              '((b . 2) (a . 1) (c . 3) (d . 4)))

                   (assert-error (pairlis 'a '(1 2)))            ;keys must be a list
                   (assert-error (pairlis '(a b) 1))             ;values must be a list
                   (assert-error (pairlis '(a) '(1 2)))          ;keys & values must be the same length
                   (assert-error (pairlis '(a b) '(1 2) 3))      ;optional 3rd argument must be an alist
                   (assert-error (pairlis (list nil a) '(1 2)))) ;keys can not be nil

         (it "can lookup"
                   (assert-eq (assoc 'a '((a . 1) (b . 2) (c . 3)))
                              '(a . 1))
                   (assert-eq (assoc 'b '((a . 1) (b . 2) (c . 3)))
                              '(b . 2))
                   (assert-eq (assoc 'c '((a . 1) (b . 2)))
                              '())

                   (assert-error (assoc 'a '(a (b . 2))))) ;second arg must be an alist (i.e. list of pairs)

         (it "can reverse lookup"
                   (assert-eq (rassoc 1 '((a . 1) (b . 2) (c . 3)))
                              '(a . 1))
                   (assert-eq (rassoc 2 '((a . 1) (b . 2) (c . 3)))
                              '(b . 2))
                   (assert-eq (rassoc 3 '((a . 1) (b . 2)))
                              '()))

         (it "can remove"
                   (assert-eq (dissoc 'a '((a . 1) (b . 2) (c . 3)))
                              '((b . 2) (c . 3)))))
