(describe reduce-with-lambda
    (== (reduce (lambda (acc item) 0 (+ acc item)) 0 '(1 2 3)) 6))
    
(describe reduce-with-primitive
    (== (reduce + 0 '(1 2 3)) 6))
    
(describe reduce-building-a-list
    (== (reduce (lambda (l i) (cons i l)) '() '(1 2 3 4)) '(4 3 2 1)))