;;; -*- mode: Scheme -*-

;;; Copyright 2015 SteelSeries ApS. All rights reserved.
;;; Use of this source code is governed by a BSD-style
;;; license that can be found in the LICENSE file.

;;; Linter

(define (expression-crawler expression car-test analyzer extract-for-recursion extract-for-analysis report)
  (cond ((nil? expression) report)
        ((atom? expression) report)
        ((list? expression)
         (if (car-test expression)
             (expression-crawler (extract-for-recursion expression)
                                 car-test
                                 analyzer
                                 extract-for-recursion
                                 extract-for-analysis
                                 (analyzer (extract-for-analysis expression) report))
             (list report
                   (expression-crawler (car expression) car-test analyzer extract-for-recursion extract-for-analysis '())
                   (expression-crawler (cdr expression) car-test analyzer extract-for-recursion extract-for-analysis '()))))
        (else report)))

;;; Analysis functions


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Look for mutator use

(define (lint:analyze-set expressions)

  (define (crawl-looking-for-set expression report)
    (cond ((nil? expression) report)
          ((atom? expression) report)
          ((list? expression)
           (if (memq (car expression) '(set! set-car! set-cdr!))
               (cons (format #f "Mutator found: ~A" expression) report)
               (list (crawl-looking-for-set (car expression) report)
                     (crawl-looking-for-set (cdr expression) report))))
          (else report)))
  
  (remove nil? (flatten* (map (lambda (ex)
                                (crawl-looking-for-set ex '()))
                              expressions))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Look for cascading initial bindings in a let or named let

(define (lint:analyze-let expressions)

  (define (back-reference-found? expr previous-bindings)
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
  
  (remove nil? (flatten* (map (lambda (ex)
                                (expression-crawler ex
                                                    (lambda (e) (eq? (car e) 'let))
                                                    analyze-let-bindings
                                                    (lambda (e) ((if (symbol? (cadr e)) cdddr cddr) e))
                                                    (lambda (e) ((if (symbol? (cadr e)) caddr cadr) e))
                                                    '()))
                              expressions))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Look for cascading initial bindings in a do

(define (lint:analyze-do expressions)

  (define (back-reference-found? expr previous-bindings)
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
      (when (back-reference-found? (cadar bindings) previous-bindings)
            (set! errors (cons (format #f "Back reference in a DO: ~A" (caar bindings)) errors)))))
  
    (remove nil? (flatten* (map (lambda (ex)
                                (expression-crawler ex
                                                    (lambda (e) (eq? (car e) 'do))
                                                    analyze-do-bindings
                                                    (lambda (e) (cddr e))
                                                    (lambda (e) (cadr e))
                                                    '()))
                              expressions))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Look for single clause ifs

(define (lint:analyze-if expressions)

  (define (analyze-if-clauses if-expr report)
    (cond ((eq? (length if-expr) 3) (format #f "Single clause IF: ~A" if-expr))
          ((nil? (caddr if-expr)) (format #f "Nil true clause IF: ~A" if-expr))
          ((nil? (cadddr if-expr)) (format #f "Nil false clause IF: ~A" if-expr))
          (else report)))

  (remove nil? (flatten* (map (lambda (ex)
                                (expression-crawler ex
                                                    (lambda (e) (eq? (car e) 'if))
                                                    analyze-if-clauses
                                                    (lambda (e) (cddr e))
                                                    (lambda (e) e)
                                                    '()))
                              expressions))))

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
    (flatten* (map (lambda (f) (f expressions))
                   (list lint:analyze-set
                         lint:analyze-let
                         lint:analyze-do
                         lint:analyze-if)))))
