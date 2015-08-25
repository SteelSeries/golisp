;;; -*- mode: Scheme -*-

(context "channels"

         (
             (define c (make-channel))
             (define buffered (make-channel 3))
             (define closed-channel (make-channel))
             (close-channel closed-channel)
         )

         (it "should work"
             (assert-eq (begin
                           (channel<- buffered 1)
                           (<-channel buffered))
                         '(1 #t)))

         (it "should act like a go channel"
             (assert-eq (begin
                           (fork (lambda (p)
                             (channel<- c 1)
                             (channel<- c 2.0)
                             (channel<- c '(3))))
                           (list (car (<-channel c)) (car (<-channel c)) (car (<-channel c))))
                         '(1 2.0 (3))))

         (it "should allow buffered channels"
             (assert-eq (begin
                           (channel<- buffered 4)
                           (channel<- buffered 5.0)
                           (channel<- buffered [6])
                           (list (car (<-channel buffered)) (car (<-channel buffered)) (car (<-channel buffered))))
                         '(4 5.0 [6])))

         (it "should validate channel buffer size"
             (assert-error (make-channel -1))
             (assert-error (make-channel 2.0)))

         (it "should validate channel type for read/write calls"
             (assert-error (<-channel 1))
             (assert-error (channel<- 1 2)))

         (it "should return false when a channel is closed and empty when read from"
             (assert-eq (<-channel closed-channel) '(() #f)))

         (it "should return true when a channel is closed but not yet empty when read from"
             (assert-eq (begin
                          (channel<- buffered 1)
                          (close-channel buffered)
                          (<-channel buffered)) '(1 #t)))

         (it "should return true when a channel is closed but not yet empty when trying to read"
             (assert-eq (begin
                          (channel<- buffered 1)
                          (close-channel buffered)
                          (channel-try-read buffered)) '(#t 1 #t)))

         (it "should return more as true when trying to read from a non-closed but empty buffer"
             (assert-eq (channel-try-read buffered) '(#f () #t)))

         (it "should error closing an already closed channel"
             (assert-error (close-channel closed-channel)))

         (it "should error writing to a closed channel"
             (assert-error (channel<- closed-channel 1))
             (assert-error (channel-try-write closed-channel 1))))
