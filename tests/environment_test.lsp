(describe global-env
          (== (nil? system-global-environment) #f))

(describe new-environment
          (define new-env (make-top-level-environment "new"))
          (== (nil? new-env) #f)
          (== (environment-has-parent? new-env) #t)
          (== (environment-parent new-env) system-global-environment))

(describe binding
          (environment-define system-global-environment 'test-name 42)
          (== (environment-bound? system-global-environment 'test-name) #t)
          (== (environment-assigned? system-global-environment 'test-name) #t)
          (== test-name 42)
          (== (environment-lookup system-global-environment 'test-name) 42))

