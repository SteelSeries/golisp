;;; -*- mode: Scheme -*-

(define when1 1)

(context "if"

         ()
         
         (it "when"
             (assert-eq (when #t 1 2 3 4) 4)
             (assert-eq (when #f 1 2 3 4) nil))

         (it "when-arbitrary-conditional"
             (assert-eq (when (< 2 4) 1 2 3 4) 4)
             (assert-eq (when (> 2 4) 1 2 3 4) nil))


         (it "when-dont-eval"
             (set! when1 1)
             (assert-eq (when #f (set! when1 42) 1) nil)
             (assert-eq when1 1))

         (it "when-eval"
             (set! when1 1)
             (assert-eq (when #t (set! when1 42) 1) 1)
             (assert-eq when1 42))



         (it "unless"
             (assert-eq (unless #f 1 2 3 4)
                        4)
             (assert-nil (unless #t 1 2 3 4)))

         (it "unless-arbitrary-conditional"
             (assert-eq (unless (> 2 4) 1 2 3 4)
                        4)
             (assert-nil (unless (< 2 4) 1 2 3 4)))


         (it "unless-dont-eval"
             (set! when1 1)
             (assert-nil (unless #t (set! when1 42) 1))
             (assert-eq when1
                        1))

         (it "unless-eval"
             (set! when1 1)
             (assert-eq (unless #f (set! when1 42) 1)
                        1)
             (assert-eq when1
                        42)))
