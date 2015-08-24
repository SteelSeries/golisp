;;; -*- mode: Scheme -*-

(context "channels"

         (
             (define c (make-channel))
             (define buffered (make-channel 3))
         )

         (it "should act like a go channel"
             (assert-eq (begin
                           (fork (lambda (p)
                             (channel<- c 1)
                             (channel<- c 2.0)
                             (channel<- c '(3))))
                           (list (<-channel c) (<-channel c) (<-channel c)))
                         '(1 2.0 (3))))

         (it "should allow buffered channels"
             (assert-eq (begin
                           (channel<- buffered 4)
                           (channel<- buffered 5.0)
                           (channel<- buffered [6])
                           (list (<-channel buffered) (<-channel buffered) (<-channel buffered)))
                         '(4 5.0 [6])))

         (it "should validate channel buffer size"
             (assert-error (make-channel -1))
             (assert-error (make-channel 2.0)))

         (it "should validate channel type for read/write calls"
             (assert-error (<-channel 1))
             (assert-error (channel<- 1 2))))
