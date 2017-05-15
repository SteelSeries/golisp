;;; Code Contract support for GoLisp
;;; Copyright 2016 Dave Astels


(if (nil? **USE_CONTRACTS**)
    (begin
      (defmacro (contract-let bindings . body) `,@body)
      (defmacro (contract-requires condition . message))
      (defmacro (contract-ensures condition . message))
      (defmacro (contract-ensures-error error-message-pattern condition)))
    (begin
      (defmacro (contract-let bindings . body)
        `(let* (,@bindings
                (ensure-clauses '())
                (ensure-error-clauses '())
                (contract-error-message '())
                (contract-result (on-error (begin ,@body)
                                           (lambda (err)
                                             (let ((msg (car (last-pair (string-split err "\n")))))
                                               (when (re-string-match-go "Contractual .+ violation" msg)
                                                 (error msg))
                                               (set! contract-error-message msg)
                                               err)))))
           (for-each eval ensure-error-clauses)
           (for-each eval ensure-clauses)
           contract-result))

      (defmacro (contract-requires condition . message)
        `(unless ,condition
           (error (if (nil? ',message)
                      (format #f "~%~%>> Contractual requirement violation")
                      (format #f "~%~%>> Contractual requirement violation: ~A" (car ',message))))))

      (defmacro (contract-ensures condition . message)
        `(set! ensure-clauses
               (cons '(unless ,condition
                        (error (if (nil? ',message)
                                   (format #f "~%~%>> Contractual assurance violation~%")
                                   (format #f "~%~%>> Contractual assurance violation: ~A~%" (car ',message)))))
                     ensure-clauses)))

      (define (contract-error-match pattern error-message)
        (re-string-match-go pattern error-message))

      (defmacro (contract-ensures-error error-message-pattern condition)
        `(set! ensure-error-clauses
               (cons '(when (not (nil? contract-error-message)) ;error occurred
                        (if (and (contract-error-match ,error-message-pattern contract-error-message)
                                 (not ,condition))
                            (error (format #f "~%~%>> Contractual assurance violation for error: ~A~%" contract-error-message))
                            (error contract-result)))
                     ensure-error-clauses)))))
