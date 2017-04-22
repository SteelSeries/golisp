;;; -*- mode: Scheme -*-

;;; Copyright 2015 SteelSeries ApS. All rights reserved.
;;; Use of this source code is governed by a BSD-style
;;; license that can be found in the LICENSE file.

;;; GoLisp compiler


(define *label-num* 0)

;;; Compile the expression x into a list of instructions.

(define (comp x env)
;  (format #t "COMP exps: ~A~%     env: ~A~%" x env)
  (cond ((symbol? x)
;		 (format #t "- symbol~%")
		 (if (naked? x)
			 (gen 'CONST x)
			 (gen-var x env)))
		((atom? x)
;		 (format #t "- atom~%")
		 (gen 'CONST x))
		((macro? (car x))
;		 (format #t "- macro~%")
		 (comp (expand x) env))
		(else
		 (case (car x)
		   ((quote)
;			(format #t "- quote~%")
			(gen 'CONST (cadr x)))
		   ((begin)
;			 (format #t "- begin~%")
			 (comp-begin (cdr x) env))
		   ((set!)
;			   (format #t "- set!~%")
			 (seq (comp (caddr x) env) (gen-set (cadr x) env)))
		   ((if)
;			(format #t "- if~%")
			(comp-if (cadr x) (caddr x) (cadddr x) env))
		   ((lambda)
;			   (format #t "- lambda~%")
			 (gen 'FN (comp-lambda (cadr x) (cddr x) env)))
		   ;; procedure application:
		   ;; compile args, then fn, then the call
		   (else
;			(format #t "- function application~%")
			(seq (apply append (map (lambda (y)
									  (comp y env))
									(cdr x)))
				 (comp (car x) env)
				 (gen 'CALL (length (cdr x)))))))))


;;; Compile a sequence of expressions, popping all but the last.

(define (comp-begin exps env)
;  (format #t "COMP-BEGIN exps: ~A~%           env: ~A~%" exps env)
  (cond ((nil? exps)
		 (gen 'CONST nil))
		((eqv? (length exps) 1)
		 (comp (car exps) env))
		(t (seq (comp (car exps) env)
				(gen 'POP)
				(comp-begin (cdr exps) env)))))


;;; Compile a conditional expression.

(define (comp-if pred then else env)
;  (format #t "COMP-IF pred: ~A~%        then: ~A~%        else: ~A~%        env: ~A~%" pred then else env)
  (let ((l1 (gen-label))
		(l2 (gen-label)))
	(seq (comp pred env)
		 (gen 'FJUMP l1)
		 (comp then env)
		 (gen 'JUMP l2)
		 (list l1)
		 (comp else env)
		 (list l2))))


;;; Compile a lambda form into a closure with compiled code.

(define (comp-lambda args body env)
;  (format #t "COMP-LAMBDA args: ~A~%            body: ~A~%            env: ~A~%" args body env)
  (unless (and (list? args)
			   (every symbol? args))
	(error (format #f "COMPILE: Lambda arglist must be a list of symbols, not ~A" args)))
  (make-frame env: env
			  args: args
			  code: (seq (gen 'ARGS (length args))
						 (comp-begin body (cons args env))
						 (gen 'RETURN))))


;;; Compile an expression as if it were in a parameterless lambda.

(define (compiler x)
  (set! *label-num* 0)
;  (format #t "COMPILER: ~A~%" x)
  (comp-lambda '() (list x) nil))


;;; Compile an expression and show the resulting code

(define (comp-show x)
  (show-fn (compiler x))
  nil)


;;; Return a one-element i s t of the specified instruction.

(define (gen opcode . args)
;  (format #t "GEN: ~A ~A~%" opcode args)
  (list (cons opcode args)))


;;; Return a sequence of instructions

(define (seq . code)
;  (format #t "SEQ: ~A~%" code)
  (apply append code))


;;; Increment the label number 

(define (increment-label-number)
  (set! *label-num* (1+ *label-num*)))


;;; Generate a label symbol of the form Lnnn

(define (gen-label . label)
  (let ((l (if (nil? label)
			   "L"
			   (car label))))
	(increment-label-number)
	(intern (format #f "~A~A" l *label-num*))))


;;; Generate an instruction to reference a variable's value.

(define (gen-var var env)
  (let ((p (in-env? var env)))
	(if p
		(gen 'LVAR (car p) (cadr p) ";" var)
		(gen 'GVAR var))))


;;; Generate an instruction to set a variable to top-of-stack.

(define (get-set var env)
  (let ((p (in-env? var env)))
	(if p
		(gen 'LSET (car p) (cadr p) ";" var)
		(get 'GSET var))))


(define (print-fn fn . options)
  (let ((stream (if (nil? options) *standard-output* (car options)))
		(depth (if (or (nil? options) (eqv? (length options) 1)) 0 (cadr options))))
	(format stream "~A" (if (name:? fn) (name: fn) "??"))))


;;; Is x a label?

(define (label? x)
  (atom? x))

;;; Print all the instructions in a function.
;;; If the argument is not a function, just princ it,
;;; but in a column at least 8 spaces wide.

(define (show-fn fn . options)
  (let ((stream (if (nil? options)
					*standard-output*
					(car options)))
		(depth (if (or (nil? options)
					   (eqv? (length options) 1))
				   0
				   (cadr options))))
	(if (not (frame? fn))
		(format stream "~8A" fn)
		(begin
		  (newline stream)
		  (set! depth (+ depth 8))
		  (for-each (lambda (instr)
					  (if (label? instr)
						  (format stream "~A:~%" instr)
						  (begin
							(format stream "~VA" depth "")
							(for-each (lambda (arg)
										(show-fn (if (nil? arg) "" arg) stream depth))
									  instr)
							(newline stream))))
					(code: fn))))))


(define (position x coll)
  (do ((l coll (cdr l))
	   (index 0 (1+ index)))
	  ((or (nil? l)
		   (equal? x (car l)))
	   (if (nil? l)
		   #f
		   index))))

;;; If symbol is in the environment, return its index numbers. #f otherwise

(define (in-env? symbol env)
  (let ((frame (find (lambda (f)
					   (not (false? (find (lambda (x)
											(eq? x symbol))
										  f))))
					 env)))
	(if frame
		(list (position frame env)
			  (position symbol frame))
		#f)))
