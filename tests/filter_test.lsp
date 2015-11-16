;;; -*- mode: Scheme -*-

(context "filter"

         ()

         (it "works"
             (assert-eq (filter even? '())
                        '())
             (assert-eq (filter even? '(1 3 5))
                        '())
             (assert-eq (filter even? '(2 4 6))
                        '(2 4 6))
             (assert-eq (filter even? '(1 2 3 4 5 6))
                        '(2 4 6))))

(context filter-errors

         ()

         (it "rejects a non-funciton predicate"
             (assert-error (filter 5 '())))
                                ;function

         (it "rejects a non-list source list"
             (assert-error (filter even? 5)))

         (it "rejects a non-boolean predicate"
             (assert-error (filter + '(1 2)))))

(context "remove"

         ()

         (it "works"
             (assert-eq (remove even? '())
                        '())
             (assert-eq (remove even? '(2 4 6))
                        '())
             (assert-eq (remove even? '(1 3 5))
                        '(1 3 5))
             (assert-eq (remove even? '(1 2 3 4 5 6))
                        '(1 3 5))))


(context remove-errors

         ()

         (it "rejects a non-function predicate"
             (assert-error (remove 5 '())))

         (it "rejects a non-list source list"
             (assert-error (remove even? 5)))

         (it "rejects a non-boolean predicate"
             (assert-error (remove + '(1 2)))))
