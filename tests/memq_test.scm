;;; -*- mode: Scheme -*-

(context "memq"

         ()

         (it "memq-with-simple-list"
             (assert-eq (memq 'a '(a b c))
                        '(a b c))
             (assert-eq (memq 'b '(a b c))
                        '(b c))
             (assert-eq (memq 'c '(a b c))
                        '(c))
             (assert-false (memq 'd '(a b c))))

         (it "memq-with-list-of-numbers"
             (assert-eq (memq 1 '(1 2 3))
                        '(1 2 3))
             (assert-eq (memq 2 '(1 2 3))
                        '(2 3))
             (assert-false (memq 4 '(1 2 3))))

         (it "find"
             (assert-eq (find even? '(3 1 4 1 5 9))
                        4)
             (assert-false (find even? '(1 3 5 7 9)))
             (assert-error (find 5 '()))   ;1st arg must be a function
             (assert-error (find + '(1 2))) ;1st arg muct be a predicate
             (assert-error (find even? 5))) ;3rd arg must be a list

         (it "find-tail"
             (assert-eq (find-tail even? '(3 1 4 1 5 9))
                        '(4 1 5 9))
             (assert-false (find-tail even? '(1 3 5 7 9)))
             (assert-error (find-tail 5 '()))   ;1st arg must be a function
             (assert-error (find-tail + '(1 2))) ;1st arg muct be a predicate
             (assert-error (find-tail even? 5))) ;3rd arg must be a list

         (it "memp"
             (assert-eq (memp even? '(3 1 4 1 5 9))
                        '(4 1 5 9))
             (assert-false (memp even? '(1 3 5 7 9))))
)
