;;; -*- mode: Scheme -*-

(describe copy-list
          (let* ((a '(1 2 3))
                 (b (copy a)))
            (set-car! a 5)
            (assert-eq a
                       '(5 2 3))
            (assert-eq b
                       '(1 2 3))))
