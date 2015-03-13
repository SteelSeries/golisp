(define when1 1)

(describe when
          (== (when #t 1 2 3 4) 4)
          (== (when #f 1 2 3 4) nil))

(describe when-arbitrary-conditional
          (== (when (< 2 4) 1 2 3 4) 4)
          (== (when (> 2 4) 1 2 3 4) nil))


(describe when-dont-eval
          (set! when1 1)
          (== (when #f (set! when1 42) 1) nil)
          (== when1 1))

(describe when-eval
          (set! when1 1)
          (== (when #t (set! when1 42) 1) 1)
          (== when1 42))



(describe unless
          (== (unless #f 1 2 3 4) 4)
          (== (unless #t 1 2 3 4) nil))

(describe unless-arbitrary-conditional
          (== (unless (> 2 4) 1 2 3 4) 4)
          (== (unless (< 2 4) 1 2 3 4) nil))


(describe unless-dont-eval
          (set! when1 1)
          (== (unless #t (set! when1 42) 1) nil)
          (== when1 1))

(describe unless-eval
          (set! when1 1)
          (== (unless #f (set! when1 42) 1) 1)
          (== when1 42))

