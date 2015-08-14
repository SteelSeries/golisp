;;; -*- mode: Scheme -*-

;;; Copyright 2015 SteelSeries ApS. All rights reserved.
;;; Use of this source code is governed by a BSD-style
;;; license that can be found in the LICENSE file.

;;; Linter


;;; Analysis functions


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Look for mutator use

(define (analyze-for-set expressions)

  (define (crawl-looking-for-set expression report)
    (cond ((nil? expression) report)
          ((atom? expression) report)
          ((list? expression)
           (if (memq (car expression) '(set! set-car! set-cdr!))
               (cons (format #f "Mutator found ~A" expression) report)
               (list (crawl-looking-for-set (car expression) report)
                     (crawl-looking-for-set (cdr expression) report))))
          (else report)))
  
  (remove nil? (flatten* (map (lambda (ex)
                                (crawl-looking-for-set ex '()))
                              expressions))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Look for cascading initial bindings in a let or named let

(define (analyze-for-bugged-let expressions)

  (define (back-reference-found? expr previous-bindings)
    (format #t "e: ~A b: ~A~%" expr previous-bindings)
    (cond ((nil? expr)
           #f)
          ((atom? expr)
           (neq? (memq expr previous-bindings) #f))
          ((list? expr)
           (or (back-reference-found? (car expr) previous-bindings)
               (back-reference-found? (cdr expr) previous-bindings)))
          (else #f)))

  (define (analyze-let-bindings bs report)
    (do ((previous-bindings '() (cons (caar bindings) previous-bindings))
         (bindings bs (cdr bindings))
         (errors report errors))
        ((nil? bindings) errors)
      (if (back-reference-found? (cadar bindings) previous-bindings)
          (set! errors (cons (format #f "Back reference in a LET: ~A" (caar bindings)) errors)))))
  
  (define (crawl-looking-for-let expression report)
    (cond ((nil? expression) report)
          ((atom? expression) report)
          ((list? expression)
           (if (eq? (car expression) 'let)
               (crawl-looking-for-let (cddr expression)
                                      (analyze-let-bindings ((if (symbol? (cadr expression)) caddr cadr) expression)
                                                            report))
               (list report
                     (crawl-looking-for-let (car expression) '())
                     (crawl-looking-for-let (cdr expression) '()))))
          (else report)))
  
  (remove nil? (flatten* (map (lambda (ex)
                                (crawl-looking-for-let ex '()))
                              expressions))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Look for cascading initial bindings in a do

(define (analyze-for-bugged-do expressions)

  (define (back-reference-found? expr previous-bindings)
    (format #t "e: ~A b: ~A~%" expr previous-bindings)
    (cond ((nil? expr)
           #f)
          ((atom? expr)
           (neq? (memq expr previous-bindings) #f))
          ((list? expr)
           (or (back-reference-found? (car expr) previous-bindings)
               (back-reference-found? (cdr expr) previous-bindings)))
          (else #f)))

  (define (analyze-do-bindings bs report)
    (do ((previous-bindings '() (cons (caar bindings) previous-bindings))
         (bindings bs (cdr bindings))
         (errors report errors))
        ((nil? bindings) errors)
      (format #t "bindings: ~A seen: ~A errors: ~A~%" bindings previous-bindings errors)
      (when (back-reference-found? (cadar bindings) previous-bindings)
            (format #t "back reference found: ~A~%" (cadar bindings))
            (set! errors (cons (format #f "Back reference in a DO: ~A" (caar bindings)) errors)))))
  
  (define (crawl-looking-for-do expression report)
    (cond ((nil? expression) report)
          ((atom? expression) report)
          ((list? expression)
           (if (eq? (car expression) 'do)
               (crawl-looking-for-do (cddr expression)
                                      (analyze-do-bindings (cadr expression) report))
               (list report
                     (crawl-looking-for-do (car expression) '())
                     (crawl-looking-for-do (cdr expression) '()))))
          (else report)))
  
  (remove nil? (flatten* (map (lambda (ex)
                                (crawl-looking-for-do ex '()))
                              expressions))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Look for single clause ifs

(define (analyze-for-single-clause-ifs expressions)
  )

;;; File processing

(define (lint filename)

  (define (read-expressions f)
    (do ((expr (read f) (read f))
         (code '() (cons expr code)))
        ((eof-object? expr) code)))
  
  
  (define (load-file filename)
    (let* ((f (open-input-file filename))
           (expressions (read-expressions f)))
      (close-port f)
      expressions))
  
  (let ((expressions (load-file filename)))
    (map (lambda (f) (f expressions))
         (list analyze-for-set
               analyze-for-bugged-let
               analyze-for-bugged-do
               analyze-for-single-clause-ifs))))
