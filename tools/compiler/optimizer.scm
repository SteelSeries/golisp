;;; -*- mode: Scheme -*-

;;; Copyright 2015 SteelSeries ApS. All rights reserved.
;;; Copyright 2017 Dave Astels
;;; Use of this source code is governed by a BSD-style
;;; license that can be found in the LICENSE file.

;;; GoLisp compiler

;;;-----------------------------------------------------------------------------
;;; Optimizer
;;; Optimizers perform surgery on the code passed them.


;;; Perform peephole optimization on assembly code.

(define **optimize** #t)				; just a hack to disable optimization


(define (optimize-function fn)
  (let ((new-code (optimize (compiled-code fn))))
	(compiled-code! fn new-code)
	fn))


(define (optimize code)
    (if **optimize**
	  (if (try-to-optimize code code)
		  (optimize code)
		  code)
	  code))


;;; Try to optimize

(define (try-to-optimize code all-code)
;  (format #t "Trying to optimize ~A~%" code)
  (cond ((nil? code)
		 #f)
		((optimize-1 code all-code)
		 #t)
		(else
		 (try-to-optimize (cdr code) all-code))))


;;; Perform peephole optimization on a tail of the assembly code.
;;; Return whether a change was made

(define (optimize-1 code all-code)
  (let* ((instr (car code))
		 (optimizer (get-optimizer (opcode instr))))
;	(if optimizer (format #t "Using optimizer for ~A~%" (opcode instr)))
	(if optimizer
	  (optimizer instr code all-code)
	  #f)))


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

(define-macro (define-optimizer opcodes args . body)
  (if (and (list? opcodes) (list? args) (= (length args) 3))
	`(for-each (lambda (op)
				 (put-optimizer op (lambda ,args ,@body)))
			   ',opcodes)
  	(error "Invalid optimizer definition")))


;;; Generate a single instruction

(define (gen1 . args)
  args)


;;; Find the code sequence that a jump statement branches to

(define (target instr code)
  (let ((found (member (arg1 instr) code)))
	(if found
	  (second found)
	  nil)))



(define (next-instr code)
  ;; Find the next actual instruction in a sequence (skipping labels)
  (let ((pair (memp (lambda (x)
					  (not (label? x)))
					code)))
	(and pair (car pair))))


(define-optimizer (LABEL) (instr code all-code)
  ;; ... L ... => ... ... ; if no reference to L
;  (format #t "instr: ~A - code: ~A~%" instr code)
  (if (false? (memp (lambda (i)
					  (eqv? (arg1 i) instr))
					all-code))
	  (begin
		(set-first! code (second code))
		(set-rest! code (rest2 code))
		#t)
	  #f))


(define-optimizer (GSET) (instr code all-code)
  ;; (gset x) (pop) (gvar x) ==> (gset x)
;  (format #t "code: ~A~%" code)
  (if (and (is (second code) 'POP)
		   (is (third code) 'GVAR)
		   (eqv? (arg1 instr) (arg1 (third code))))
	(begin
	  (set-rest! code (rest3 code))
	  #t)
	#f))


(define-optimizer (LSET) (instr code all-code)
  ;; (lset n m) (pop) (lvar n m) ==> (lset n m)
;  (format #t "code: ~A~%" code)
  (if (and (is (second code) 'POP)
		   (is (third code) 'LVAR)
		   (eqv? (arg1 instr) (arg1 (third code)))
		   (eqv? (arg2 instr) (arg2 (third code))))
	(begin
	  (set-rest! code (rest3 code))
	  #t)
	#f))


(define-optimizer (JUMP) (instr code all-code)
  ;; (JUMP L1) ...dead code... L2 ==> (JUMP L1) L2
  (let ((next-label (memp label? (cdr code))))
	(when (and next-label
			   (neqv? next-label (second code)))
	  (set-rest! code next-label)))
  
  (cond
   ;; (JUMP L1) L1 ... ==> ...
   ((eqv? (arg1 instr) (second code))
	(set-car! code (second code))
	(set-cdr! code (rest2 code))) 
   ;; (JUMP L1) ... L1 (JUMP L2) ==> (JUMP L2) ... L1 (JUMP L2)
   ((and (is (target instr code) 'JUMP)
		 (neqv? (arg1 instr) (arg1 (target instr code))))
	(set-second! instr (arg1 (target instr code)))
	#t)
   (else
	#f)))


(define-optimizer (TJUMP FJUMP) (instr code all-code)
  ;; (FJUMP L1) ... L1 (JUMP L2) ==> (FJUMP L2 ... L1 (JUMP L2)
  (if (is (target instr code) 'JUMP)
	(begin
	  (set-second! instr (arg1 (target instr code)))
	  #t)
	#f))


(define-optimizer (TRUE ZERO ONE TWO NIL) (instr code all-code)
  (case (opcode (second code))
	((NOT) ;; (TRUE) (NOT) ==> (FALSE)
	 (set-first! code (gen1 'FALSE))
	 (set-rest! code (rest2 code))
	 #t)
	((FJUMP) ;; (TRUE) (FJUMP L) ... ==> ...
	 (set-first! code (third code))
	 (set-rest! code (rest3 code))
	 #t)
	((TJUMP) ;; (TRUE) (TJUMP L) ... ==> (JUMP L) ...
	 (set-first! code (gen1 'JUMP (arg1 (second code))))
	 (set-rest! code (rest2 code))
	 #t)
	(else
	 #f)))


(define-optimizer (FALSE) (instr code all-code)
  (case (opcode (second code))
	((NOT) ;; (FALSE) (NOT) ==> (TRUE)
	 (set-first! code (gen1 'TRUE))
	 (set-rest! code (rest2 code))
	 #t)
	((TJUMP) ;; (FALSE) (TJUMP L) ... ==> ...
	 (set-first! code (third code))
	 (set-rest! code (rest3 code))
	 #t)
	((FJUMP) ;; (FALSE) (FJUMP L) ==> (JUMP L)
	 (set-first! code (gen1 'JUMP (arg1 (second code))))
	 (set-rest! code (rest2 code))
	 #t)
	(else
	 #f)))



