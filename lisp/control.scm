;;; -*- mode: Scheme -*-

;;; Copyright 2015 SteelSeries ApS. All rights reserved.
;;; Copyright 2017 Dave Astels. All rights reserved.
;;; Use of this source code is governed by a BSD-style
;;; license that can be found in the LICENSE file.


(define-macro (let bindings . body)
  (if (and (list? bindings)
  			   (every (lambda (x)
  						(and (list? x)
  							 (eqv? (length x) 2)
  							 (symbol? (car x))))
  					  bindings))
	`((lambda ,(map car bindings) ,@body) ,@(map cadr bindings))
	(error "let bindings must be a list of two elements lists, the first of which is a symbol")))


(define-macro (let* bindings . body)
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

;;; remove support until I can figure out the compilation
;; (define-macro (letrec bindings . body)
;;   `(let ,(map (lambda (binding)
;; 				(list (car binding) nil))
;; 			  bindings)
;; 	 ,@(map (lambda (binding)
;; 			  `(set! ,@binding))
;; 			bindings)
;; 	 ,@body))
