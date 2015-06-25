;;; -*- mode: Scheme -*-

(describe begin
          (assert-eq (begin 4)
                     4)
          (assert-eq (begin 1 2)
                     2))
