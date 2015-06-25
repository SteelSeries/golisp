;;; -*- mode: Scheme -*-

(describe global-env
          (assert-not-nil (system-global-environment)))

(describe new-environment
          (define new-env (make-top-level-environment "new"))
          (assert-not-nil new-env)
          (assert-true (environment-has-parent? new-env))
          (assert-eq (environment-parent new-env)
                     (system-global-environment)))

(describe binding
          (environment-define (system-global-environment) 'test-name 42)
          (assert-true (environment-bound? (system-global-environment) 'test-name))
          (assert-true (environment-assigned? (system-global-environment) 'test-name))
          (assert-eq test-name
                     42)
          (assert-eq (environment-lookup (system-global-environment) 'test-name)
                     42))

