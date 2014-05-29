(describe equals
  (== (eq? (list) 42) #f)
  (== (eq? 42 (list)) #f)
  (== (eq? (alist '((a.1))) 42) #f)
  (== (eq? (car (alist '((a.1)))) 42) #f)
  (== (eq? 42 "42") #f)
  (== (eq? (alist '((a.1))) (alist '((a.1) (b.2)))) #f)
  (== (eq? '(1 2) '(1 2 3)) #f))
