;;; -*- mode: Scheme -*-

(context "concurrency"

         ()

         (it "should wait for the task to finish on join"
             (define x 1)
             (assert-eq (begin
                          (define f (fork (lambda (proc) (set! x 2) 3)))
                          (join f)
                          x)
                        2))

         (it "should return the task return value on join"
             (define x 1)
             (assert-eq (begin
                          (define f (fork (lambda (proc) (set! x 2) 3)))
                          (join f))
                        3))

         (it "should error when trying to join twice"
             (define x 1)
             (assert-error (begin
                          (define f (fork (lambda (proc) ())))
                          (join f)
                          (join f))))

         (it "shouldn't allow resetting or abandoning non-scheduled tasks"
             (define f (fork (lambda (proc) ())))
             (define s (schedule 1 (lambda (proc) ())))
             (assert-error (reset-timeout f))
             (assert-error (abandon f))
             (assert-nerror (reset-timeout s))
             (assert-nerror (abandon s))))
