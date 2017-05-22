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
  (log-it "assemble-function")
;  (dump-fn fn)
  (let* ((code (compiled-code fn))
		 (first-pass-result (asm-first-pass code))
		 (len (first first-pass-result))
		 (labels (second first-pass-result))
		 (second-pass-result (asm-second-pass code len labels))
		 (bytecode (convert-to-bytecode second-pass-result)))
	(log-it "Bytecode: ~A~%" bytecode)
	(compiled-code! fn bytecode)))


;;; Return the labels and the total code length

(define (harvest-labels code len labels)
  (cond ((nil? code)
		 (list len labels))
		((label? (car code))
		 (harvest-labels (cdr code) len (acons (car code) len labels)))
		(else
		 (harvest-labels (cdr code) (1+ len) labels))))


(define (asm-first-pass code)
  (log-it "asm-first-pass")
  (harvest-labels code 0 '()))


;;; Put code into code-vector, adjusting for labels

(define (asm-second-pass code len labels)
  (log-it "asm-second-pass: ~A" code)
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
	code-vector))


;;; Convert the assembly code into bytecode vectors

(define (convert-to-bytecode code)
  (log-it "Converting to bytecode")
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



