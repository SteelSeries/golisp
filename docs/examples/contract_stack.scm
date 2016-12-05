(define Stack {
  new: (lambda ()
         {proto*: Stack
                  storage: '()})
       
  empty?: (lambda ()
            (nil? storage))

  peek: (lambda ()
          (car storage))
  
  push: (lambda (x)
          (contract-let ()
            (contract-requires (not (nil? x)) "Attempt to push nil")
            (contract-ensures (equal? (peek) x))
            (storage:! self (cons x storage))))
  
  pop: (lambda ()
         (contract-let ((old-storage storage))
           (contract-requires (not (empty?)) "Attempt to pop an empty stack")
           (contract-ensures (not (nil? contract-result)) "Nil was found on the stack and never should be")
           (contract-ensures (eqv? (length storage) (-1+ (length old-storage))) "Popping did not remove an element")
           (let ((x (car storage)))
             (storage:! self (cdr storage))
             x)))

  })
