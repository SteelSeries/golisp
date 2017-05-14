;;; -*- mode: Scheme -*-

;;; Copyright 2015 SteelSeries ApS. All rights reserved.
;;; Copyright 2017 Dave Astels
;;; Use of this source code is governed by a BSD-style
;;; license that can be found in the LICENSE file.

;;; GoLisp compiler - assembler

;;;-----------------------------------------------------------------------------
;;; Assembler


;;; Turn a list of instructions into a vector

(define (assemble-function fn)
  (log-it "assemble ~A" fn)
  (let* ((code (compiled-code fn))
		 (first-pass-result (asm-first-pass code))
		 (len (first first-pass-result))
		 (labels (second first-pass-result))
		 (second-pass-result (asm-second-pass code len labels)))
	(compiled-code! fn second-pass-result)
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
					(log-it "assoced: ~A" (assoc (arg1 instr) labels))
					(set-arg1! instr (cdr (assoc (arg1 instr) labels))))
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
  (and (not (label? instr))
	   (if (list? op)
		   (memv (car instr) op)
		   (eqv? (car instr) op))))



