;;; -*- mode: Scheme -*-

(describe do
          (assert-eq (do ((l '(1 2 3) (cdr l))
                          (c 0 (+ c 1))
                          (b 10))
                         ((nil? l) (list b c))
                       (set! b (+ b 1)))
                     '(13 3)))
