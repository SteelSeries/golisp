;;; -*- mode: Scheme -*-

(context "concurrency"

         (
             (define x 1)
             (define f (fork (lambda (proc) (set! x 2) 3)))
             (define s (schedule 0 (lambda (proc) ())))
         )

         (it "should wait for the task to finish on join"
             (assert-eq (begin
                          (join f)
                          x)
                        2))

         (it "should return the task return value on join"
             (assert-eq (join f) 3))

         (it "should error when trying to join twice"
             (assert-error (begin
                             (join f)
                             (join f))))

         (it "shouldn't allow resetting or abandoning non-scheduled tasks"
             (assert-error (reset-timeout f))
             (assert-error (abandon f))
             (assert-nerror (reset-timeout s))
             (assert-nerror (abandon s))))
