;;; -*- mode: Scheme -*-

;;; Copyright 2015 SteelSeries ApS. All rights reserved.
;;; Use of this source code is governed by a BSD-style
;;; license that can be found in the LICENSE file.

;;; Conditionals are all defined in terms of if


(define-macro (when condition-expr . body)
  `(if ,condition-expr
     (begin ,@body)
     nil))


(define-macro (unless condition-expr . body)
  `(if ,condition-expr
     nil
     (begin ,@body)))


(define-macro (cond . clauses)
  (if (nil? clauses)
    nil
    (if (pair? (car clauses))
      (if (eqv? (caar clauses) 'else)
        `(begin ,@(cdar clauses))
        `(if ,(caar clauses)
           (begin ,@(cdar clauses))
           (cond ,@(cdr clauses))))
      (error "cond clauses must be lists"))))


(define-macro (case key . clauses)
  (let ((key-val (gensym "KEY")))
    `(let ((,key-val ,key))
       (cond ,@(map (lambda (clause)
                      (if (eqv? (car clause) 'else)
                        clause
                        `((member ,key-val ',(car clause))
                          ,@(cdr clause))))
                    clauses)))))


(define-macro (and . args)
  (if (nil? args)
    #t
    (if (eqv? (length args) 1)
      (car args)
      `(if ,(car args)
         (and ,@(cdr args))
         #f))))


(define-macro (or . args)
  (if (nil? args)
    #f
    (if (eqv? (length args) 1)
      (car args)
      (let ((var (gensym)))
        `(let ((,var ,(car args)))
           (if ,var
             ,var
             (or ,@(cdr args))))))))



