;;; -*- mode: Scheme -*-

(define when1 1)

(describe when
          (assert-eq (when #t 1 2 3 4) 4)
          (assert-eq (when #f 1 2 3 4) nil))

(describe when-arbitrary-conditional
          (assert-eq (when (< 2 4) 1 2 3 4) 4)
          (assert-eq (when (> 2 4) 1 2 3 4) nil))


(describe when-dont-eval
          (set! when1 1)
          (assert-eq (when #f (set! when1 42) 1) nil)
          (assert-eq when1 1))

(describe when-eval
          (set! when1 1)
          (assert-eq (when #t (set! when1 42) 1) 1)
          (assert-eq when1 42))



(describe unless
          (assert-eq (unless #f 1 2 3 4)
                     4)
          (assert-nil (unless #t 1 2 3 4)))

(describe unless-arbitrary-conditional
          (assert-eq (unless (> 2 4) 1 2 3 4)
                     4)
          (assert-nil (unless (< 2 4) 1 2 3 4)))


(describe unless-dont-eval
          (set! when1 1)
          (assert-nil (unless #t (set! when1 42) 1))
          (assert-eq when1
                     1))

(describe unless-eval
          (set! when1 1)
          (assert-eq (unless #f (set! when1 42) 1)
                     1)
          (assert-eq when1
                     42))

