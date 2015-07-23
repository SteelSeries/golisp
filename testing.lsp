;;; -*- mode: Scheme -*-

;;; Copyright 2015 SteelSeries ApS. All rights reserved.
;;; Use of this source code is governed by a BSD-style
;;; license that can be found in the LICENSE file.

(define number-of-passes 0)
(define number-of-failures 0)
(define number-of-errors 0)

(define failure-messages '())
(define error-messages '())

(define verbose-tests nil)
(define context-name "")
(define it-name "")


(define (reset-testing )
  (set! number-of-passes 0)
  (set! number-of-failures 0)
  (set! number-of-errors 0)
  (set! failure-messages '())
  (set! error-messages '())
  (set! verbose-tests nil))

(define (log-pass msg)
  (set! number-of-passes (succ number-of-passes))
  (when verbose-tests
    (format #t "    ~A - ok~%" msg)))


(define (log-failure prefix msg)
  (set! number-of-failures (succ number-of-failures))
  (let ((failure-message (format #f "~A ~A: ~A - ~A" context-name it-name prefix msg)))
    (set! failure-messages (cons failure-message failure-messages))
    (when verbose-tests
          (format #t "    ~A - failed: ~A~%" prefix msg))))

(define (log-error err)
  (set! number-of-errors (succ number-of-errors))
  (set! error-messages (cons err error-messages))
  (when verbose-tests
        (format #t "   error: ~A~%" err)))

(defmacro (context label setup . body)
  (if (not (or (symbol? label) (string? label)))
      (error "The label of a describe must be a symbol or string.")
      `(begin (when verbose-tests
                (format #t "~A~%" ,label))
              (set! context-name ,label)
              (for-each (lambda (it-clause)
                          ,@setup
                          (eval it-clause))
                        ',body))))

(defmacro (it label . body)
  (if (not (or (symbol? label) (string? label)))
      (error "The label of a describe must be a symbol or string.")
      `(begin (when verbose-tests
                (format #t "~%  ~A~%" ,label))
              (set! it-name ,label)
              (on-error (begin ,@body)
                        (lambda (err) (log-error err))))))

(defmacro (assert-true sexpr)
  `(let ((actual (eval ,sexpr))
         (msg (format #f "(assert-true ~A)" ',sexpr)))
     (if actual
         (log-pass msg)
         (log-failure msg "expected true, but was false"))))


(defmacro (assert-false sexpr)
  `(let ((actual (eval ,sexpr))
         (msg (format #f "(assert-false ~A)" ',sexpr)))
     (if (not actual)
         (log-pass msg)
         (log-failure msg "expected false, but was true"))))


(defmacro (assert-nil sexpr)
  `(let ((actual (eval ,sexpr))
         (msg (format #f "(assert-null ~A)" ',sexpr)))
     (if (nil? actual)
         (log-pass msg)
         (log-failure msg "expected nil, but wasn't"))))


(defmacro (assert-not-nil sexpr)
  `(let ((actual (eval ,sexpr))
         (msg (format #f "(assert-not-null ~A)" ',sexpr)))
     (if (not (nil? actual))
         (log-pass msg)
         (log-failure msg "expected not nil, but was"))))


(defmacro (assert-eq sexpr expected-sexpr)
  `(let* ((actual ,sexpr)
          (expected ,expected-sexpr)
          (msg (format #f "(assert-eq ~A ~A)" ',sexpr ',expected-sexpr)))
     (if (eq? actual expected)
         (log-pass msg)
         (log-failure msg (format #f "expected ~A, but was ~A" expected actual)))))


(defmacro (assert-neq sexpr expected-sexpr)
  `(let* ((actual ,sexpr)
          (expected ,expected-sexpr)
          (msg (format #f "(assert-neq ~A ~A)" ',sexpr ',expected-sexpr)))
     (if (neq? actual expected)
         (log-pass msg)
         (log-failure msg (format #f "did not expect ~A, but it was" expected)))))

(defmacro (assert-error **sexpr**)
  `(let ((msg (format #f "(assert-error ~A)" ',**sexpr**)))
     (on-error ,**sexpr**
               (lambda (err)
                 (log-pass msg))
               (lambda ()
                 (log-failure msg "expected an error, but there wasn't")))))

(define (dump-summary duration)
  (format #t "~%Ran ~A tests in ~A seconds~%"
          (+ number-of-passes number-of-failures number-of-errors)
          (/ duration 1000.0))
  (format #t "~A passes, ~A failures, ~A errors~%"
          number-of-passes
          number-of-failures
          number-of-errors)
  (unless (zero? number-of-failures)
    (format #t "~%Failures:~%")
    (for-each (lambda (m) (format #t "  ~A~%" m))
              failure-messages))
  (unless (zero? number-of-errors)
    (format #t "~%Errors:~%")
    (for-each (lambda (m) (format #t "  ~A~%" m))
              error-messages)))

(define (run-all-tests test-dir . optionals)
  (reset-testing)
  (set! verbose-tests (not (nil? optionals)))
  (let ((t (time (for-each load (list-directory test-dir "*_test.lsp")))))
    (dump-summary t)))

(define (run-test test-file . optionals)
  (reset-testing)
  (set! verbose-tests (not (nil? optionals)))
  (let ((t (time (load test-file))))
    (dump-summary t)))

