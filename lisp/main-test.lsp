(define (main args)
  (map write-line args)
  (apply + (map string->number args)))
