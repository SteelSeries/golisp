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

(context "atomic"

         (
             (define x (atomic))
         )

         (it "should initialize with integers"
             (assert-eq (atomic-load (atomic 25)) 25))

         (it "should set the new value if the old value matches"
             (assert-eq (begin
                          (atomic-compare-and-swap! x 0 1)
                          (atomic-load x))
                        1))

         (it "should not set the new value if the old value doesn't match"
             (assert-eq (begin
                          (atomic-compare-and-swap! x 2 3)
                          (atomic-load x))
                        0))

         (it "should return the correct values"
             (assert-true (atomic-compare-and-swap! x 0 1))
             (assert-false (atomic-compare-and-swap! x 2 3)))

         (it "should only accept integer values"
             (assert-error (atomic 0.0))
             (assert-error (atomic-compare-and-swap! x 0.0 1))
             (assert-error (atomic-compare-and-swap! x 0 1.0)))

         (it "should require an atomic object as the first parameter"
             (assert-error (atomic-compare-and-swap! 0 0 1))))
