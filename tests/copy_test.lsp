(describe copy-list
          (let* ((a '(1 2 3))
                 (b (copy a)))
            (set-car! a 5)
            (== a '(5 2 3))
            (== b '(1 2 3))))
