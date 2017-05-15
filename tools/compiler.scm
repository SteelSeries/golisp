;;; -*- mode: Scheme -*-

;;; Copyright 2015 SteelSeries ApS. All rights reserved.
;;; Copyright 2017 Dave Astels
;;; Use of this source code is governed by a BSD-style
;;; license that can be found in the LICENSE file.

;;; GoLisp compiler


(define (make-path filename)
  (string-join (list (get-env "GOLISPHOME") "tools" "compiler" filename) "/"))

(load (make-path "utils.scm"))
(load (make-path "compiler.scm"))
(load (make-path "assembler.scm"))
(load (make-path "optimizer.scm"))


;;; Build a new function.

(define (optimize-and-assemble-function f)
  (assemble-function (optimize-function f)))


(define (create-compiled-function name env args code)
  (optimize-and-assemble-function (new-fn name env args code)))

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
  (let ((stream (if (nil? options)
					*standard-output*
					(car options)))
		(indent (if (or (nil? options) (nil? (cdr options)))
				   0
				   (cadr options))))
	(if (not (compiled-function? fn))
		(format stream "~8A" fn)
		(begin
		  (newline stream)
		  (for-each (lambda (i) 
					  (let* ((instr (vector-ref (compiled-code fn) i))
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
					(iota (vector-length (compiled-code fn))))))))


;;; Compile an expression and show the resulting code

(define (comp-show x)
  (let ((fn (compile x)))
	(format #t "Result:~%")
	(show-fn fn)
	nil))

(define (run x)
  (execute (compile x)))





;;;-----------------------------------------------------------------------------
;;; Loading bytecode


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


(define (load-bytecode fname)
  (let* ((f (open-input-file fname))
		 (raw-code (read f)))
	(close-port f)
	(map execute (reconstitute-code raw-code))))


;;;-----------------------------------------------------------------------------
;;; Compiling a file of source code


(define (save-code fname code)
  (let ((bytecode-file (open-output-file fname)))
	(format bytecode-file "~S" code)
	(close-port bytecode-file)))


(define (compile-sexprs source-file result)
  (let ((sexpr (read source-file)))
	(if (eof-object? sexpr)
	  result
	  (compile-sexprs source-file (cons (compile sexpr) result)))))


(define (compile-file fname)
  (format #t "Compiling ~A~%" fname)
  (let* ((source-file (open-input-file fname))
		 (compiled-code (compile-sexprs source-file '())))
	(close-port source-file)
	(save-code (str fname "c") compiled-code)
	nil))


