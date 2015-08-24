;;; -*- mode: Scheme -*-

(context "channels"

         (
             (define c (make-channel))
         )

         (it "should act like a go channel"
             (assert-eq (begin
                           (fork (lambda (p)
                             (chan<- c 1)
                             (chan<- c 2.0)
                             (chan<- c '(3))))
                           (list (<-chan c) (<-chan c) (<-chan c)))
                         '(1 2.0 (3))))

         (it "should validate channel buffer size"
             (assert-error (make-channel -1))
             (assert-error (make-channel 2.0))))
