;;; -*- mode: Scheme -*-

;;; Copyright 2015 SteelSeries ApS. All rights reserved.
;;; Use of this source code is governed by a BSD-style
;;; license that can be found in the LICENSE file.

;;; GoLisp compiler


;;;-----------------------------------------------------------------------------
;;; Logging

(define **LOGGING** #f)

(define (logging on/off)
  (set! **LOGGING** on/off))

(define (log-it format-string . objects)

  (when **LOGGING**
	(apply format (if (nil? objects)
					  (list #t format-string)
					  (cons* #t format-string objects)))
	(newline)))

;;;-----------------------------------------------------------------------------
;;; The compiler

(define opcode-symbols '(ZERO ONE TWO TRUE FALSE NIL LVAR LSET GVAR GSET POP CONST JUMP FJUMP TJUMP SAVE RETURN CALLJ ARGS ARGSDOT FN PRIM SET_CC CC CAR CDR CADR NOT NILP LIST1 RANDOM PLUS MINUS TIMES DIVIDE LT GT LTEQ GTEQ CONS LIST2 EQV NEQV EQ NEQ EQUAL NEQUAL LIST3 NAMEBANG HALT))

(define opcodes (do ((syms opcode-symbols (cdr syms))
					 (index 0 (1+ index))
					 (code-list '() (acons (car syms) index code-list)))
					((nil? syms)
					 code-list)))

(define *label-num* 0)

;;; Compile the expression x into a list of instructions.

(define (comp x env val? more?)
  (log-it "COMP exps: ~A~%     env: ~A" x env)
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
		((macro? (eval (first x)))
		 (log-it "- macro")
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
				  (comp (cons* 'set! formals body))
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
			(comp-funcall (first x) (rest x) env val? more?))))))


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


;;; Compile an application of a function to arguments

(define (comp-funcall f args env val? more?)
  (let ((prim (primitive-op? f env (length args))))
	(cond (prim							; function compilable to a primitive instruction
		   (log-it "Compiling prim")
		   (if (and (false? val?)
					(false? (prim-side-effects prim)))
			   ;; side-effect free primitive when value unused
			   (begin
				 (log-it "no side effect, and not used - just compiling args")
				 (com-begin args env #f more?))
			   ;; primitive with value or call needed
			   (begin
				 (log-it "compiling call")
				 (seq (comp-list args env)
					  (gen (prim-opcode prim))
					  (unless val?
						(gen 'POP))
					  (unless more?
						(gen 'RETURN))))))
		  ((special-form? (eval f))
		   (log-it "Compiling special form")
		   (seq (map (lambda (arg)
					   (gen 'CONST arg))
					 args)
				;(comp f env #t #t)
				(gen 'PRIM f (length args))
				(when (false? val?)
				  (gen 'POP))))
		  ((primitive? (eval f))
		   (log-it "Compiling primitive function")
		   (seq (comp-list args env)
				;; (comp f env #t #t)
				(gen 'PRIM f (length args))
				(when (false? val?)
				  (gen 'POP))
				(unless more?
				  (gen 'RETURN))))
		  ((and (starts-with? f 'lambda)
				(nil? (second f)))
		   ;; ((lambda () body) ==> (begin body)
		   (unless (nil? args)
			 (error "Too many arguments supplied"))
		   (comp-begin (cddr f) env val? more?))
		  (more?						; need to save the continuation point
		   (let ((k (gen-label 'K)))
			 (seq (gen 'SAVE k)
				  (comp-list args env)
				  (comp f env #t #t)
				  (gen 'CALLJ (length args))
				  (list k)
				  (when (false? val?)
					(gen 'POP)))))
		  (else							; function call as rename plus goto
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
;;; by somethign in the environment, and has the right number of args.

(define (primitive-op? f env n-args)
  (and (false? (in-env? f env))
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
								  (new-fn env: nil
										  name: (prim-symbol prim)
										  code: (seq (gen 'PRIM (prim-symbol prim))
													 (gen 'RETURN)))))
			*primitive-fns*))


;;; Compile a lambda form into a closure with compiled code.

(define (comp-lambda args body env)
  (log-it "COMP-LAMBDA args: ~A~%            body: ~A~%            env: ~A" args body env)
  (new-fn name: "anon"
		  env: env
		  args: args
		  code: (seq (gen-args args 0)
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


;;; Build a new function.

(define (new-fn . stuff)
  (let ((fn-data (apply make-frame stuff)))
	(assemble (make-frame env: (get-slot fn-data env:)
						  name: (get-slot-or-nil fn-data name:)
						  args: (get-slot-or-nil fn-data args:)
						  code: (optimize (get-slot fn-data code:))))))


;;; Compile an expression as if it were in a parameterless lambda.

(define (compile x)
  (set! *label-num* 0)
  (log-it "COMPILER: ~A" x)
  (comp-lambda '() (list x) nil))


;;; Compile an expression and show the resulting code

(define (comp-show x)
  (show-fn (compiler x))
  nil)


;;; Return a one-element i s t of the specified instruction.

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

;;;-----------------------------------------------------------------------------
;;; Assembler


;;; Is x a label?

(define (label? x)
  (symbol? x))



;;; Turn a list of instructions into a vector

(define (assemble fn)
  (log-it "assemble ~A" fn)
  (let* ((result (asm-first-pass (get-slot fn code:)))
		 (len (first result))
		 (labels (second result)))
	(set-slot! fn code: (asm-second-pass (get-slot fn code:) len labels))
	fn))


;;; Return the labels and the total code length

(define (asm-first-pass code)
  (log-it "asm-first-pass")
  (let ((len 0)
		(labels nil))
	(for-each (lambda (instr)
				(if (label? instr)
					(set! labels (acons instr len labels))
					(set! len (1+ len))))
			  code)
	(list len labels)))


;;; Put code into code-vector, adjusting for labels

(define (asm-second-pass code len labels)
  (log-it "asm-second-pass")
  (let ((addr 0)
		(code-vector (make-vector len)))
	(for-each (lambda (instr)
				(log-it "~A" instr)
				(unless (label? instr)
				  (when (is instr '(JUMP TJUMP FJUMP SAVE))
					(log-it "label: ~A" (nth 1 instr))
					(log-it "labels: ~A" labels)
					(log-it "assoced: ~A" (assoc (nth 1 instr) labels))
					(set-nth! 1 instr (cdr (assoc (nth 1 instr) labels))))
				  (vector-set! code-vector addr instr)
				  (set! addr (1+ addr))))
			  code)
	(convert-to-bytecode code-vector)))


;;; Convert the assembly code into bytecode vectors

(define (convert-to-bytecode code)
  (vector-map (lambda (instr)
				(let ((bytecode (assoc (car instr) opcodes)))
				  (if (false? bytecode)
					  (error (format #f "Invalid opcode: ~A" (car instr)))					  
					  (list->vector (cons (cdr bytecode) (cdr instr))))))
			  code))

;;; True if instr's opcode is OP, or one of OP if OP is a list

(define (is instr op)
  (if (list? op)
	  (memv (car instr) op)
	  (eqv? (car instr) op)))


;;;-----------------------------------------------------------------------------
;;; Optimizer

;;; Helpers

(define opcode first)
(define arg1 second)
(define arg3 third)


;;; Perform peephole optimization on assembly code.

(define (optimize code)
  ;; optimize each tail
  (if (do ((code-tail code (cdr tail))
		   (changed? #f (or (optimize-1 code-tail code)
							changed?)))
		  ((nil? code-tail) changed?))
	(optimize code)
	code))


;;; Perform peephole optimization on a tail of the assembly code.
;;; Return whether a change was made

(define (optimize-1 code all-code)
  (let* ((instr (car code))
		 (optimizer (get-optimizer (opcode instr))))
	(when (optimizer)
	  (optimizer instr code all-code))))


;;; Optimizer management using a alist

(define optimizers '())

;;; Get the assembly language optimizer for this opcode.

(define (get-optimizer opcode)
  (let ((pair (assoc opcode optimizers)))
	(and pair (cdr pair))))


;;; Store an assembly language optimizer for this opcode.

(define (put-optimizer opcode fn)
  (set! optimizers (acons opcode fn optimizers)))


;;; Define assembly language optimizers for these opcodes.

(define-macro (def-optimizer opcodes args . body)
  (unless (and (list? opcodes) (list? args) (= (length args) 3))
	(error "Invalid optimizer definition"))
  `(for-each (lambda (op)
			   (put-optimizer op (lambda ,args ,@body)))
			 ,opcodes))


;;; Generate a single instruction

(define (gen1 . args)
  args)


;;; Find the code sequence that a jump statement branches to

(define (target instr code)
  (second (member (arg1 instr) code)))


;;; Find the next actual instruction in a sequence (skipping labels)

(define (next-instr code)
  (let ((pair (memp (lambda (x)
					  (not (label? x)))
					code)))
	(and pair (car pair))))


;;; ... L ... => ... ... ; if no reference to L

(def-optimizer (LABEL) (instr code all-code)
  (let ((label (arg1 instr)))
	(if (not (memp (lambda (i)
					   (eqv? (arg1 i) label))))
	  (begin
		(set-car! code (cadr code))
		(set-cdr! code (cddr code))
		#t)
	  #f)))


;;;-----------------------------------------------------------------------------
;;; Print a compiled function

;;; Format the code index to be right aligned, indented appropriately

(define (format-code-index i indent)
  (let ((prefix (cond ((< i 10) "  ")
					  ((< i 100) " ")
					  (else ""))))
	(format #f "~VA~A~A: " indent "" prefix i)))


;;; Print all the instructions in a function.
;;; If the argument is not a function, just princ it,
;;; but in a column at least 8 spaces wide.

(define (show-fn fn . options)
  ;(log-it "show-fn")
  (let ((stream (if (nil? options)
					*standard-output*
					(car options)))
		(indent (if (or (nil? options) (nil? (cdr options)))
				   0
				   (cadr options))))
	(if (not (frame? fn))
		(format stream "~8A" fn)
		(begin
		  (newline stream)
		  (for-each (lambda (i)
					  (let* ((instr (vector-ref (get-slot fn code:) i))
							 (instr-length (vector-length instr)))
						(format stream (format-code-index i indent))
						(vector-for-each (lambda (arg)
										   (show-fn arg stream (+ indent 8)))
										 (subvector instr 0 (min (list instr-length 3))))
						(format stream "~VA; ~A ~A"
								(vector-ref #(0 16 8 0 0) instr-length) ""
								(car (rassoc (vector-ref instr 0) opcodes))
								(if (eqv? instr-length 4) (vector-ref instr 3) ""))
						(newline stream)))
					(iota (vector-length (get-slot fn code:))))))))




;;;-----------------------------------------------------------------------------
;;; Compiled code serialization

(define (save-code code)
  (format #t "~S" ))


(define (reconstitute-code raw)
  (format #t "reconstituting ~A~%" raw)
  (cond ((nil? raw)
		 nil)
		((list? raw)
		 (if (eqv? (car raw) 'make-frame)
			 (apply make-frame (reconstitute-code (cdr raw)))
			 (map reconstitute-code raw)))
		((vector? raw)
		 (vector-map reconstitute-code raw))
		(else
		 raw)))

(define (load-code fname)
  (let* ((f (open-input-file fname))
		 (raw-code (read f)))
	(close-port f)
	(reconstitute-code raw-code)))


(define (run x)
  (execute (compile x)))

