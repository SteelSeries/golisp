(define (run proc)
  (do ((woken #f woken))
      (woken (write-line "woken"))
    (write-line "tick")
    (set! woken (proc-sleep proc 10000))))


(define (run-forever proc)
  (do ()
      ()
      (write-line "tick")
      (sleep 10000)))


(define (run-once proc)
  (write-line "start")
  (sleep 1000)
  (write-line "stop"))

(define (run-delayed proc)
  (write-line "running"))
