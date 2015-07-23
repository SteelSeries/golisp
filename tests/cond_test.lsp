;;; -*- mode: Scheme -*-

(context "cond"

         ()
         
         (it "works when the first test is true"
                   (assert-eq (cond (#t 1)
                                    (#f 2))
                              1))

         (it "works when the secone clause is true"
             (assert-eq (cond (#f 1)
                              (#t 2))
                        2))

         (it "works with the else clause"
             (assert-eq (cond (#f 1)
                              (#f 2)
                              (else 3))
                        3))

         (it "errors when a clause isn't a pair"
             (assert-error (cond #t 1
                                 (#t 2))))

         (it "evals the body clauses and results in the last"
             (assert-eq (cond (#f 1)
                              (#t 1 2 3))
                        3)
             (assert-eq (cond (#f 1 2 3)
                              (#f 4 5 6)
                              (else 7 8 9))
                        9)))
