;;; -*- mode: Scheme -*-

;;; Copyright 2015 SteelSeries ApS. All rights reserved.
;;; Copyright 2017 Dave Astels
;;; Use of this source code is governed by a BSD-style
;;; license that can be found in the LICENSE file.

;;; GoLisp compiler - compiler


(define opcode-symbols '(ZERO			; 0
						 ONE			; 1
						 TWO			; 2
						 TRUE			; 3
						 FALSE			; 4
						 NIL			; 5
						 LVAR			; 6
						 LSET			; 7
						 GVAR			; 8
						 GSET			; 9
						 POP			; 10
						 CONST			; 11
						 JUMP			; 12
						 FJUMP			; 13
						 TJUMP			; 14
						 SAVE			; 15
						 RETURN			; 16
						 CALLJ			; 17
						 ARGS			; 18
						 ARGSDOT		; 19
						 FN				; 20
						 PRIM			; 21
						 FUNC			; 22
						 SET_CC			; 23
						 CC				; 24
						 CAR			; 25
						 CDR			; 26
						 CADR			; 27
						 NOT			; 28
						 NILP			; 29
						 LIST1			; 30
						 RANDOM			; 31
						 PLUS			; 32
						 MINUS			; 33
						 TIMES			; 34
						 DIVIDE			; 35
						 LT				; 36
						 GT				; 37
						 LTEQ			; 38
						 GTEQ			; 39
						 CONS			; 40
						 LIST2			; 41
						 EQV			; 42
						 NEQV			; 43
						 EQ				; 44
						 NEQ			; 45
						 EQUAL			; 46
						 NEQUAL			; 47
						 LIST3			; 48
						 NAMEBANG		; 49
						 HALT))			; 50

(define (make-opcodes syms code mapping)
  (if (nil? syms)
	  mapping
	  (make-opcodes (cdr syms) (1+ code) (acons (car syms) code mapping))))

(define opcodes (make-opcodes opcode-symbols 0 '()))


(define *label-num* 0)

;;; Compile the expression x into a list of instructions.

(define (comp x env val? more?)
  (log-it "COMP expr: ~A~%     env: ~A" x env)
  (cond ((member x (list #t #f nil 0 1 2))
		 (comp-const x val? more?))
		((symbol? x)
		 (log-it "- symbol")
		 (if (naked? x)
		   (comp-const x val? more?)
		   (comp-var x env val? more?)))
		((atom? x)
		 (log-it "- atom")
		 (comp-const x val? more?))
		((macro? (eval (car x) ***COMPILER-ENVIRONMENT***))
		 (log-it "- macro: ~A" (car x))
		 (comp (apply expand x) env val? more?))
		(else
		 (case (car x)
		   ((quote)
			(log-it "- quote")
			(arg-count x 1)
			(gen 'CONST (second x)))
		   ((begin)
			(log-it "- begin")
			(comp-begin (rest x) env val? more?))
		   ((define)
			(log-it "- define")
			(let ((formals (cadr x))
				  (body (cddr x)))
			  (if (symbol? formals)
				(comp (cons* 'set! formals body) env val? more?)
				(let ((name (first formals))
					  (params (rest formals)))
				  (comp (list 'name! (list 'set! name (cons* 'lambda params body)) (list 'quote name)) env val? more?)
				  ))))
		   ((set!)
			(log-it "- set!")
			(arg-count x 2)
			(unless (symbol? (cadr x))
			  (error (format #f "Only symbols can be set!, not ~A in ~A" (cadr x) x)))
			(seq (comp (third x) env #t #t)
				 (gen-set (second x) env)
				 (unless val?
				   (gen 'POP))
				 (unless more?
				   (gen 'RETURN))))
		   ((if)
			(log-it "- if")
			(arg-count x 2 3)
			(comp-if (second x) (third x) (fourth x) env val? more?))
		   ((lambda)
			(log-it "- lambda")
			(when val?
			  (let ((f (comp-lambda (second x) (cddr x) env)))
				(seq (gen 'FN f)
					 (unless more?
					   (gen 'RETURN))))))
		   ;; procedure application:
		   ;; compile args, then fn, then the call
		   (else
			(log-it "- function application: ~A" x)
			(let ((desugarred (desugar x)))
			  (comp-funcall (first desugarred) (rest desugarred) env val? more?)))))))


;;; Report an error if form has the wrong number of args

(define (arg-count form . limits)
  (let ((n-args (length (rest form)))
		(min-args (if (nil? limits) 0 (first limits)))
		(max-args (if (or (nil? limits) (nil? (cdr limits))) 100 (second limits))))
	(unless (<= min-args n-args max-args)
	  (error (format #f "Wrong number of arguments for ~A in ~A: ~A supplied, ~A to ~A expected"
					 (first form) form n-args min-args max-args)))))


;;; Compile a sequence of expressions, popping all but the last.

(define (comp-begin exps env val? more?)
  (log-it "COMP-BEGIN exps: ~A~%           env: ~A" exps env)
  (cond ((nil? exps)
		 (comp-const nil val? more?))
		((eqv? (length exps) 1)
		 (comp (first exps) env val? more?))
		(t (seq (comp (first exps) env #f #t)
				(comp-begin (rest exps) env val? more?)))))


;;; Compile a list, leaving them all on the stack

(define (comp-list exps env)
  (if (nil? exps)
	  nil
	  (seq (comp (first exps) env #t #t)
		   (comp-list (rest exps) env))))


;;; Compile a constant expression

(define (comp-const x val? more?)
  (log-it "comp-const: ~A" x)
  (when val?
	(seq (cond ((eqv? x #t)
				(gen 'TRUE))
			   ((eqv? x #f)
				(gen 'FALSE))
			   ((nil? x)
				(gen 'NIL))
			   ((eqv? x 0)
				(gen 'ZERO))
			   ((eqv? x 1)
				(gen 'ONE))
			   ((eqv? x 2)
				(gen 'TWO))
			   (else
				(gen 'CONST x)))
		 (unless more?
		   (gen 'RETURN)))))


;;; Compile a variable reference

(define (comp-var x env val? more?)
  (when val?
	(seq (gen-var x env)
		 (unless more?
		   (gen 'RETURN)))))


;;; Compile a conditional expression.

(define (comp-if pred then else env val? more?)
  (log-it "COMP-IF pred: ~A~%        then: ~A~%        else: ~A~%        env: ~A val?=~A more?=~A" pred then else env val? more?)
  (cond ((false? pred)					; (if #f x y) ==> y
		 (comp else env val? more?))
		((atom? pred)				; (if #t x y) ==> x
		 (comp then env val? more?))
		((and (list? pred)				; (if (not p) x y) ==> (if p y x)
			  (eqv? (length (rest pred)) 1)
			  (primitive-op? (first pred) env 1)
			  (eq? (prim-opcode (primitive-op? (first pred) env 1)) 'not))
		 (comp-f (second pred) else then env val? more?))
		(else
		 (let ((pcode (comp pred env #t #t))
			   (tcode (comp then env val? more?))
			   (ecode (comp else env val? more?)))
		   (cond ((equal? tcode ecode)	; (if p x x) ==> (begin p x)
				  (seq (comp pred env #f #t) ecode))
				 ((nil? tcode)			; (if p nil y) ==> p (TJUMP L2) y L2:
				  (let ((l2 (gen-label)))
					(seq pcode
						 (gen 'TJUMP l2)
						 ecode
						 (list l2)
						 (unless more?
						   (gen 'RETURN)))))
				 ((nil? ecode)			; (if p x) ==> p (FJUMP l1) x L1:
				  (let ((l1 (gen-label)))
					(seq pcode
						 (gen 'FJUMP l1)
						 tcode
						 (list l1)
						 (unless more?
						   (gen 'RETURN)))))
				 (else					; (if p x y) ==> p (FJUMP L1) x L1: y
										;             or p (FJUMP L1) x (JUMP L2) L1: y L2:
				  (let ((l1 (gen-label))
						(l2 (when more?
							  (gen-label))))
					(seq pcode
						 (gen 'FJUMP l1)
						 tcode
						 (when more?
						   (gen 'JUMP l2))
						 (list l1)
						 ecode
						 (when more?
						   (list l2))))))))))


(define (starts-with? l x)
  (and (pair? l)
	   (eq? (first l) x)))


;;; Desugar channel & frame functions

(define (desugar expr)
  (let* ((func-sym (car expr))
		 (result (if (symbol? func-sym)
					 (let* ((func-name (symbol->string func-sym))
							(name-length (string-length func-name)))
					   (cond ((< name-length 2)
							  expr)
							 ((naked? func-sym)
								 (cons* 'get-slot (cadr expr) func-sym (cddr expr)))
							 ((eq? (string-ref func-name (- name-length 2)) #\:)
							  (let ((final (string-ref func-name (-1+ name-length)))
									(slot (intern (string-head func-name (-1+ name-length))))
									(frame-val (cadr expr))
									(args (cddr expr)))
								(cond ((eq? final #\!)
									   (cons* 'set-slot! frame-val slot args))
									  ((eq? final #\?)
									   (cons* 'has-slot? frame-val slot args))
									  ((eq? final #\>)
									   (cons* 'send frame-val slot args))
									  ((eq? final #\^)
									   (cons* 'send-super slot (cdr expr))))))
							 ((string=? (substring func-name 0 2) "<-")
							  (cons* 'channel-read (intern (string-tail func-name 2)) (cdr expr)))
							 ((string=? (substring func-name (- name-length 2) name-length) "<-")
							  (cons* 'channel-write (intern (string-head func-name (- name-length 2))) (cdr expr)))
							 (else
							  expr)))
					 expr)))
	(log-it "Desuggaring ~A -> ~A" expr result)
	result))


;;; Compile an application of a function to arguments

(define (comp-funcall f args env val? more?)
  (let* ((prim (primitive-op? f env (length args))))
	(log-it "Compiling a function call: ~A~%" f)
;	(log-it "(car f) is ~A" (eval (car f)))

	;; a function compilable to a primitive instruction 
	(cond (prim							
		   (log-it "Compiling prim")
		   (if (and (false? val?)
					(false? (prim-side-effects prim)))
			   ;; side-effect free primitive when value unused
			   (begin
				 (log-it "no side effect, and not used - just compiling args")
				 (com-begin args env #f more?))
			   ;; primitive with value or call needed
			   (begin
				 (log-it "compiling the call")
				 (seq (comp-list args env)
					  (gen (prim-opcode prim))
					  (unless val?
						(gen 'POP))
					  (unless more?
						(gen 'RETURN))))))

		  ;; a lambda with no args
		  ;; ((lambda () body) ==> (begin body)
		  ((and (list? f)
				(eqv? (car f) 'lambda)		
				(nil? (cadr f)))
		   (log-it "Compiling a no arg lambda")
		   (unless (nil? args)
			 (error "Too many arguments supplied"))
		   (comp-begin (cddr f) env val? more?))

		   ;; a special form
		  ((special-form? (eval f))
		   (log-it "Compiling special form call")
		   (seq (map (lambda (arg)
					   (gen 'CONST arg))
					 args)
										;(comp f env #t #t)
				(gen 'PRIM f (length args))
				(when (false? val?)
				  (gen 'POP))))

		  ;; a primitive (implemented directly in Go)
		  ((primitive? (eval f))		
		   (log-it "Compiling primitive function call")
		   (seq (comp-list args env)
				;; (comp f env #t #t)
				(gen 'PRIM f (length args))
				(when (false? val?)
				  (gen 'POP))
				(unless more?
				  (gen 'RETURN))))

		  ;; an interpreted function
		  ((function? (eval f))			
		   (log-it "Compiling an interpreted function call")
		   (seq (comp-list args env)
				;; (comp f env #t #t)
				(gen 'FUNC f (length args))
				(when (false? val?)
				  (gen 'POP))
				(unless more?
				  (gen 'RETURN))))

		  ;; a compiled function call with code following: need to save the continuation point
		  (more?						
		   (log-it "Compiling an compiled function call")
		   (let ((k (gen-label 'K)))
			 (seq (gen 'SAVE k)
				  (comp-list args env)
				  (comp f env #t #t)
				  (gen 'CALLJ (length args))
				  (list k)
				  (when (false? val?)
					(gen 'POP)))))

		  ;; a compiled function call at the end - as rename plus goto
		  (else							
		   (log-it "Compiling a tail recursive compiled function call")
		   (seq (comp-list args env)
				(comp f env #t #t)
				(gen 'CALLJ (length args)))))))


(define *primitive-fns*
  '((+ 2 PLUS #t) (- 2 MINUS #t) (* 2 TIMES #t) (/ 2 DIVIDE #t)
	(< 2 LT) (> 2 GT) (<= 2 LTE) (>= 2 GTE)
	(eq? 2 EQ) (neq? 2 NEQ) (equal? 2 EQUAL) (nequal? 2 NEQUAL) (eqv? 2 EQV) (neqv? 2 NEQV)
	(not 1 NOT) (nil? 1 NILP)
	(car 1 CAR) (cdr 1 CDR) (cadr 1 CADR) (cons 2 CONS #t)
	(list 1 LIST1 #t) (list 2 LIST2 #t) (list 3 LIST3 #t)
	(name! 2 NAMEBANG #t)))


;;; F is a primitive is it is in the table, and is not shadowed
;;; by something in the environment, and has the right number of args.

(define (primitive-op? f env n-args)
  (and (symbol? f)
	   (false? (in-env? f env))
	   (find (lambda (prim)
			   (and (equal? f (prim-symbol prim))
					(eqv? n-args (prim-n-args prim))))
			 *primitive-fns*)))


(define (prim-symbol prim)
  (first prim))

(define (prim-n-args prim)
  (second prim))

(define (prim-opcode prim)
  (third prim))

(define (prim-always prim)
  (fourth prim))

(define (prim-side-effects prim)
  (fifth prim))


;;; Initialize the primitive functions.

(define (init-scheme-comp)
  (for-each (lambda (prim)
			  (environment-define (system-global-environment)
								  (prim-symbol prim)
								  (create-compiled-function (prim-symbol prim)
															nil
															nil
															(seq (gen 'PRIM (prim-symbol prim))
																 (gen 'RETURN)))))
			*primitive-fns*))


;;; Compile a lambda form into a closure with compiled code.

(define (comp-lambda args body env)
  (log-it "COMP-LAMBDA args: ~A~%            body: ~A~%            env: ~A" args body env)
  (create-compiled-function "anon"
							env
							args
							(seq (gen-args args 0)
								 (comp-begin body (cons (make-true-list args) env) #t #f))))


;;; Generate an instruction to load the arguments

(define (gen-args args n-so-far)
  (cond ((nil? args)
		 (gen 'ARGS n-so-far))
		((symbol? args)
		 (gen 'ARGS. n-so-far))
		((and (pair? args)
			  (symbol? (first args)))
		 (gen-args (rest args) (1+ n-so-far)))
		(else
		 (error "Illegal argument list"))))


;;; Convert a possibly dotted list into a true, non-dotted list.

(define (make-true-list dotted-list)
  (cond ((nil? dotted-list)
		 nil)
		((atom? dotted-list)
		 (list dotted-list))
		(else
		 (cons (first dotted-list)
			   (make-true-list (rest dotted-list))))))

;;; Return a one-element list of the specified instruction & args.

(define (gen opcode . args)
  (log-it "GEN: ~A ~A" opcode args)
  (list (cons opcode args)))


;;; Return a sequence of instructions

(define (seq . code)
  (log-it "SEQ: ~A" code)
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
		(gen 'LVAR (car p) (cadr p))
		(gen 'GVAR var))))


;;; Generate an instruction to set a variable to top-of-stack.

(define (gen-set var env)
  (let ((p (in-env? var env)))
	(if p
		(gen 'LSET (car p) (cadr p))
		(if (assoc var *primitive-fns*)
			(error (format #f "Can't alter the constant ~A" var))
			(gen 'GSET var)))))


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


;;; Build a new function.

(define (optimize-and-assemble-function f)
;  (dump-fn f)
  (assemble-function (optimize-function f)))


(define (create-compiled-function name env args code)
  (if (and (list? code)
		   (eqv? (length code) 0))
	(error "setting code to empty"))
  (optimize-and-assemble-function (make-compiled-function name env args code)))

;;;-----------------------------------------------------------------------------
;;; Print a compiled function

;;; Format the code index to be right aligned, indented appropriately

(define (format-code-index i indent)
  (let ((prefix (cond ((< i 10) "  ")
					  ((< i 100) " ")
					  (else ""))))
	(format #f "~VA~A~A: " indent "" prefix i)))


										;  5: 16                              ; RETURN

;;; Print all the instructions in a function.
;;; If the argument is not a function, just princ it,
;;; but in a column at least 8 spaces wide.

(define (show-fn fn . options)
  (let ((stream (if (nil? options)
				  *standard-output*
				  (car options)))
		(indent (if (or (nil? options) (nil? (cdr options)))
				  0
				  (cadr options))))
	(if (not (compiled-function? fn))
	  (format stream "~8A" fn)
	  (let ((code (compiled-code fn)))
		(newline stream)
		(vector-for-each (lambda (i instr) 
						   (let* ((instr-length (vector-length instr)))
							 (format stream (format-code-index i indent))
							 (vector-for-each (lambda (arg)
												(show-fn arg stream (+ indent 8)))
											  (subvector instr 0 (min (list instr-length 3))))
							 (format stream "~VA; ~A ~A ~A ~A"
									 (if (eqv? 20 (vector-first instr))
										 37
										 (+ (- 8 indent) (vector-ref #(0 16 8 0 0) instr-length)))
									 ""
									 (car (rassoc (vector-ref instr 0) opcodes))
									 (if (> instr-length 1) (vector-ref instr 1) "")
									 (if (> instr-length 2) (vector-ref instr 2) "")
									 (if (> instr-length 3) (vector-ref instr 3) ""))
							 (newline stream)))
						 (list->vector (interval 0 (-1+ (vector-length code))))
						 code)))))


(define (dump-fn fn)
  (format #t
		  "=>Compiled function:~%=>  Name: ~A~%=>  Args: ~A~%=>  Env: ~A~%=>  Code: ~A~%"
		  (compiled-name fn)
		  (compiled-args fn)
		  (compiled-env fn)
		  (compiled-code fn)))


;;; Compile an expression and show the resulting code

(define (comp-show x)
  (let ((fn (compile x)))
;	(dump-fn fn)
	(format #t "Result:~%")
	(eval (list show-fn fn))
	nil))


