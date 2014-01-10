(describe timing
          (let ((t (time (sleep 2000))))
            (>= t 2000)))

(describe apply
          (== (apply + '(1 2)) 3))
