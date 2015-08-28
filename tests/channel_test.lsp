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
                           (channel-write buffered 1)
                           (channel-read buffered))
                         '(1 #t)))

         (it "should act like a go channel"
             (assert-eq (begin
                           (fork (lambda (p)
                             (channel-write c 1)
                             (channel-write c 2.0)
                             (channel-write c '(3))))
                           (list (car (channel-read c)) (car (channel-read c)) (car (channel-read c))))
                         '(1 2.0 (3))))

         (it "should allow buffered channels"
             (assert-eq (begin
                           (channel-write buffered 4)
                           (channel-write buffered 5.0)
                           (channel-write buffered [6])
                           (list (car (channel-read buffered)) (car (channel-read buffered)) (car (channel-read buffered))))
                         '(4 5.0 [6])))

         (it "should validate channel buffer size"
             (assert-error (make-channel -1))
             (assert-error (make-channel 2.0)))

         (it "should validate channel type for read/write calls"
             (assert-error (channel-read 1))
             (assert-error (channel-write 1 2)))

         (it "should return false when a channel is closed and empty when read from"
             (assert-eq (channel-read closed-channel) '(() #f)))

         (it "should return true when a channel is closed but not yet empty when read from"
             (assert-eq (begin
                          (channel-write buffered 1)
                          (close-channel buffered)
                          (channel-read buffered)) '(1 #t)))

         (it "should return true when a channel is closed but not yet empty when trying to read"
             (assert-eq (begin
                          (channel-write buffered 1)
                          (close-channel buffered)
                          (channel-try-read buffered)) '(#t 1 #t)))

         (it "should return more as true when trying to read from a non-closed but empty buffer"
             (assert-eq (channel-try-read buffered) '(#f () #t)))

         (it "should error closing an already closed channel"
             (assert-error (close-channel closed-channel)))

         (it "should error writing to a closed channel"
             (assert-error (channel-write closed-channel 1))
             (assert-error (channel-try-write closed-channel 1)))

         (it "should handle shortcuts"
             (assert-eq (begin
                           (buffered<- 1)
                           (<-buffered))
                        '(1 #t)))

         (it "should not accept strings for shortcuts"
             (assert-error ("buffered<-" 1))
             (assert-error ("<-buffered"))))
