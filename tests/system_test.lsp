;;; -*- mode: Scheme -*-

(describe timing
          (let ((t (time (sleep 2000))))
            (>= t 2000)))

(describe apply
          (assert-eq (apply + '(1 2))
                     3))
