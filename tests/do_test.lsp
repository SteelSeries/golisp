;;; -*- mode: Scheme -*-

(context do

         ()

         (it "works"
             (assert-eq (do ((l '(1 2 3) (cdr l))
                             (c 0 (+ c 1))
                             (b 10))
                            ((nil? l) (list b c))
                          (set! b (+ b 1)))
                        '(13 3)))

         (it "supports optional step"
             (assert-eq (do ((a 1 (+ a 1))
                             (b 0))
                            ((eq? a 5) (cons a b)))
                        '(5 . 0)))

         (it "rejects non-list bindings"
             (assert-error (do 4 (#t) (+ 1 2))))

         (it "rejects non-nested-lists bindings"
             (assert-error (do (1 2) (#t) (+ 1 2))))

         (it "rejects non-symbol binding names"
             (assert-error (do ((1 2)) (#t) (+ 1 2))))

         (it "rejects non-list test"
             (assert-error (do ((x 1)) #t (+ 1 2)))))
