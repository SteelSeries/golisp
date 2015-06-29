;;; -*- mode: Scheme -*-

(describe do
          (assert-eq (do ((l '(1 2 3) (cdr l))
                          (c 0 (+ c 1))
                          (b 10))
                         ((nil? l) (list b c))
                       (set! b (+ b 1)))
                     '(13 3))

          (assert-error (do 4 (#t) (+ 1 2))) ;1st arg bindings have to be a list
          (assert-error (do (1 2) (#t) (+ 1 2))) ;bindings have to be a list of lists
          (assert-error (do ((1 2)) (#t) (+ 1 2))) ;binding name must be a symbol
          (assert-error (do ((x 1)) #t (+ 1 2))) ;2nd arg (the test) must be a list
          )
