;;; -*- mode: Scheme -*-

(context "acons"

         ()
         
         (it "adds a pair to an empty alist"
             (assert-eq (acons 'a 1 '())
                        '((a . 1))))
         
         (it "defaults the exising alist to the empty alist"
             (assert-eq (acons 'a 1)
                        '((a . 1))))

         (it "adds a pair to a non-empty alist"
             (assert-eq (acons 'a 1 '((b . 2) (c . 3)))
                        '((a . 1) (b . 2) (c . 3))))
         
         (it "prepends the new pair"
             (assert-eq (acons 'a 5 '((a . 1) (b . 2) (c . 3)))
                        '((a . 5) (a . 1) (b . 2) (c . 3))))

         (it "raises an error if the key is a list"
             (assert-error (acons '(1 2) 1 '()))))

(context "pairlis"

         ()
         
         (it "creates an alist from lists of keys and values"
             (assert-eq (pairlis '(a b) '(1 2))
                        '((b . 2) (a . 1))))
         
         (it "prepends to an exising alist"
             (assert-eq (pairlis '(a b) '(1 2) '((c . 3) (d . 4)))
                        '((b . 2) (a . 1) (c . 3) (d . 4))))

         (it "raises an error when given keys that aren't a list"
             (assert-error (pairlis 'a '(1 2))))
         
         (it "raises an error when given values that aren't a list"
             (assert-error (pairlis '(a b) 1)))
         
         (it "raises an error if key and value lists are of different lengths"
             (assert-error (pairlis '(a) '(1 2))))
         
         (it "raises an error if the proivded alist isn't an alist"
             (assert-error (pairlis '(a b) '(1 2) 3)))
         
         (it "raises an error if given nil keys"
             (assert-error (pairlis (list nil a) '(1 2)))))

(context "can be searched"

         ()
         
         (it "finds the pair with the given key"
             (assert-eq (assoc 'a '((a . 1) (b . 2) (c . 3)))
                        '(a . 1)))
         
         (it "finds another pair with a different key"
             (assert-eq (assoc 'b '((a . 1) (b . 2) (c . 3)))
                        '(b . 2)))
         
         (it "finds the first pair with the given key"
             (assert-eq (assoc 'b '((a . 1) (b . 2) (b . 3)))
                        '(b . 2)))
         
         (it "returns an empty pair if given a key that isn't in the alist"
             (assert-eq (assoc 'c '((a . 1) (b . 2)))
                        '()))

         (it "raises an error if given a non-alist to search"
             (assert-error (assoc 'a '(a (b . 2))))))

(context "can be searched by value"

         ()
         
         (it "finds a pair with the given value"
             (assert-eq (rassoc 1 '((a . 1) (b . 2) (c . 3)))
                        '(a . 1)))
         
         (it "finds another pair with a different value"
             (assert-eq (rassoc 2 '((a . 1) (b . 2) (c . 3)))
                        '(b . 2)))

         (it "finds the first pair with a given value"
             (assert-eq (rassoc 2 '((a . 2) (b . 2)))
                        '(a . 2)))
         
         (it "returns an empty pair if given a value that isn't in the alist"
             (assert-eq (rassoc 3 '((a . 1) (b . 2)))
                        '())))

(context "can can have pairs removed"

         ()
         
         (it "returns an alist without the first pair with the given key"
             (assert-eq (dissoc 'a '((a . 1) (b . 2) (a . 3)))
                        '((b . 2)))))
