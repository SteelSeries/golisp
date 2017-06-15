;;; -*- mode: Scheme -*-

;;; Copyright 2015 SteelSeries ApS. All rights reserved.
;;; Copyright 2017 Dave Astels
;;; Use of this source code is governed by a BSD-style
;;; license that can be found in the LICENSE file.

;;; Compiler macros.
;;; ONLY TO BE LOADED BY THE COMPILER

(define-compiler-macro (cond . clauses)
  (if (nil? clauses)
    nil
    (if (pair? (car clauses))
      (if (eqv? (caar clauses) 'else)
        `(begin ,@(cdar clauses))
        `(if ,(caar clauses)
           (begin ,@(cdar clauses))
           (cond ,@(cdr clauses))))
      (error "cond clauses must be lists"))))


(define-compiler-macro (case key . clauses)
  (let ((key-val (gensym "KEY")))
    `(let ((,key-val ,key))
       (cond ,@(map (lambda (clause)
                      (if (eqv? (car clause) 'else)
                        clause
                        `((member ,key-val ',(car clause))
                          ,@(cdr clause))))
                    clauses)))))


(define-compiler-macro (and . args)
  (if (nil? args)
    #t
    (if (eqv? (length args) 1)
	  (car args)
      `(if ,(car args)
         (and ,@(cdr args))
         #f))))


(define-compiler-macro (or . args)
  (if (nil? args)
    #f
    (if (eqv? (length args) 1)
      (car args)
      (let ((var (gensym)))
        `(let ((,var ,(car args)))
           (if ,var
             ,var
             (or ,@(cdr args))))))))


(define-compiler-macro (let bindings . body)
  (if (and (list? bindings)
  			   (every (lambda (x)
  						(and (list? x)
  							 (eqv? (length x) 2)
  							 (symbol? (car x))))
  					  bindings))
	`((lambda ,(map car bindings) ,@body) ,@(map cadr bindings))
	(error "let bindings must be a list of two elements lists, the first of which is a symbol")))


(define-compiler-macro (let* bindings . body)
  (if (and (list? bindings)
		   (every (lambda (x)
					(and (list? x)
						 (eqv? (length x) 2)
						 (symbol? (car x))))
				  bindings))
	(if (nil? bindings)
	  `(begin ,@body)
	  `(let (,(first bindings))
		 (let* ,(cdr bindings) ,@body)))
	(error "let bindings must be a lists of two elements lists, the first of which is a symbol")))


;;; leave as special form until I can figure out the compilation
;; (define-macro (letrec bindings . body)
;;   `(let ,(map (lambda (binding)
;; 				(list (car binding) nil))
;; 			  bindings)
;; 	 ,@(map (lambda (binding)
;; 			  `(set! ,@binding))
;; 			bindings)
;; 	 ,@body))
