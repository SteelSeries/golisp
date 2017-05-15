(define Account {
  supports-overdraft?: #t
                                      
  overdraft-limit: 0
                                      
  balance: 0
                                      
  new: (lambda (opening-amount overdraft? limit)
         (contract-let ()
           (contract-requires (> opening-amount (- limit)) "Opening amount exceeds overdraft")
           (contract-requires (if overdraft? (> limit 0) (eq? limit 0)) "bad overdraft amount")
           (contract-requires (<= limit 1000) "overdraft is too big")
           (contract-requires (>= limit 0) "negative overdraft")
           
           (contract-ensures (eq? balance opening-amount) "balance incorrectly set")
           (contract-ensures (eq? overdraft-limit limit) "overdraft incorrectly set")
           
           (balance:! self opening-amount)
           (supports-overdraft?:! self overdraft?)
           (overdraft-limit:! self limit)))
                                      
  deposit: (lambda (amount)
             (contract-let ()
               (contract-requires (> amount 0) "negative deposit amount")
               (balance:! self (+ balance amount))))
                                      
  withdraw: (lambda (amount)
              (contract-let ((old-balance balance))
                (contract-requires (<= amount (+ balance overdraft-limit)) "Amount causes overdraft to be exceeded")
                (contract-ensures (eq? balance (- old-balance amount)) "incorrect balance calculation")
                (balance:! self (- balance amount))))
  
  })
