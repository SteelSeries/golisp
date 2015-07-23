;;; -*- mode: Scheme -*-

(context "environment support"

         ()
         
         (it "lets you get the global environment"
             (assert-not-nil (system-global-environment)))

         (it "lets you create a new environment"
             (define new-env (make-top-level-environment "new"))
             (assert-not-nil new-env)
             (assert-true (environment-has-parent? new-env))
             (assert-eq (environment-parent new-env)
                        (system-global-environment)))

         (it "lets you create and query bindings"
             (environment-define (system-global-environment) 'test-name 42)
             (assert-true (environment-bound? (system-global-environment) 'test-name))
             (assert-true (environment-assigned? (system-global-environment) 'test-name))
             (assert-eq test-name
                        42)
             (assert-eq (environment-lookup (system-global-environment) 'test-name)
                        42))


         (it "throws errors as expected"
             (assert-error (environment-has-parent? 5))
             (assert-error (environment-parent 5))
             (assert-error (environment-bound-names 5))
             (assert-error (environment-macro-names 5))
             (assert-error (environment-bindings 5))
             (assert-error (environment-reference-type? 5 'a))
             (assert-error (environment-reference-type? (system-global-environment) 5))
             (assert-error (environment-bound? 5 'a))
             (assert-error (environment-bound? (system-global-environment) 5))
             (assert-error (environment-assigned? 5 'a))
             (assert-error (environment-assigned? (system-global-environment) 5))
             (assert-error (environment-assigned? (system-global-environment) '____foobar))

             (defmacro (test-macro) )
             (assert-error (environment-assigned? (system-global-environment) 'test-macro))
             
             (assert-error (environment-lookup 5 'a))
             (assert-error (environment-lookup (system-global-environment) 5))
             (assert-error (environment-lookup (system-global-environment) '____foobar))
             (assert-error (environment-lookup-macro 5 'a))
             (assert-error (environment-lookup-macro (system-global-environment) 5))
             (assert-error (environment-assignable? 5 'a))
             (assert-error (environment-assignable? (system-global-environment) 5))
             (assert-error (environment-assign! 5 'a 1))
             (assert-error (environment-assign! (system-global-environment) 5 1))
             (assert-error (environment-definable? 5 'a))
             (assert-error (environment-definable? (system-global-environment) 5))
             (assert-error (environment-define 5 'a 1))
             (assert-error (environment-define (system-global-environment) 5 1))
             (assert-error (let () (let () (the-environment))))
             (assert-error (make-top-level-environment 1 2)) ;not a string name first, and not a binding list
             (assert-error (make-top-level-environment '(1 2))) ;not symbol binding names
             (assert-error (make-top-level-environment '(a b) 5)) ;not a list of binding values
             (assert-error (make-top-level-environment '(a b) '(1 2 3))) ;different length names & values
             (assert-error (make-top-level-environment '(3 4) '(1 2))) ;not symbol binding names
             (assert-error (procedure-environment +)))) ;not a user defined function
