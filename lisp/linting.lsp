;;; -*- mode: Scheme -*-

;;; Copyright 2015 SteelSeries ApS. All rights reserved.
;;; Use of this source code is governed by a BSD-style
;;; license that can be found in the LICENSE file.

;;; Linter


;;; Analysis functions

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

(define (analyze-for-bugged-let expressions)

  (define (analyze-let-bindings bs report)
    (do ((bindings bs (cdr bindings))
         (previous-bindings '() (cons (caar bindings)))
         (errors report errors))
        ((nil? bindings) errors)
      (if (memq (caar bindings) previous-bindings)
          (set! errors (cons (format #f "Back reference in a LET: ~A" (caar bindings)))))))
  
  (define (crawl-looking-for-let expression report)
    (cond ((nil? expression) report)
          ((atom? expression) report)
          ((list? expression)
           (if (eq? (car expression) 'let)
               (crawl-looking-for-let (cddr expression)
                                      (analyze-let-bindings (cadr expression) report))
               (list (crawl-looking-for-let (car expression) report)
                     (crawl-looking-for-let (cdr expression) report))))
          (else report)))
  
  (remove nil? (flatten* (map (lambda (ex)
                                (crawl-looking-for-set ex '()))
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
    (map (lambda (f) (f expressions))
         (list analyze-for-set)))))
