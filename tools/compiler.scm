;;; -*- mode: Scheme -*-

;;; Copyright 2015 SteelSeries ApS. All rights reserved.
;;; Copyright 2017 Dave Astels
;;; Use of this source code is governed by a BSD-style
;;; license that can be found in the LICENSE file.

;;; GoLisp compiler driver


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
		(begin
		  (newline stream)
		  (for-each (lambda (i) 
					  (let* ((instr (vector-ref (compiled-code fn) i))
							 (instr-length (vector-length instr)))
						(format stream (format-code-index i indent))
						(vector-for-each (lambda (arg)
										   (show-fn arg stream (+ indent 8)))
										 (subvector instr 0 (min (list instr-length 3))))
						(format stream "~VA; ~A ~A ~A ~A"
								(if (eqv? 20 (vector-first instr))
								  37
								  (+ (- 8 indent) (vector-ref #(0 16 8 0 0) instr-length))) ""
								(car (rassoc (vector-ref instr 0) opcodes))
								(if (> instr-length 1) (vector-ref instr 1) "")
								(if (> instr-length 2) (vector-ref instr 2) "")
								(if (> instr-length 3) (vector-ref instr 3) ""))
						(newline stream)))
					(iota (vector-length (compiled-code fn))))))))


;;; Compile an expression and show the resulting code

(define (comp-show x)
  (let ((fn (compile x)))
	(format #t "Result:~%")
	(show-fn fn)
	nil))


;;; Compile an expression and execute the resulting code

(define (run x)
  (execute (compile x)))


;;;-----------------------------------------------------------------------------
;;; Loading bytecode


(define (reconstitute-code raw)
  ;; (format #t "reconstituting ~A: ~A~%" (type-of raw) raw)
  (cond ((or (eqv? 'nil raw) (nil? raw))
		 nil)
		((list? raw)
		 (if (eqv? (car raw) 'make-frame)
			 (apply make-frame (reconstitute-code (cdr raw)))
			 (map reconstitute-code raw)))
		((vector? raw)
		 (vector-map reconstitute-code raw))
		(else
		 raw)))


(define (compress-code expanded-code)
  (vector-map (lambda (c)
				(if (eqv? (vector-first c) 20) ; FN?
				  (let ((m (vector-second c)))
					(vector 20 (make-compiled-function (name: m)
													   (env: m)
													   (args: m)
													   (compress-code (code: m)))))
				  c))
			  expanded-code))


(define (frame->compiled-function frame)
  ;; (format #t "~A~%" frame)
  (make-compiled-function (name: frame)
						  (env: frame)
						  (args: frame)
						  (compress-code (code: frame))))


(define (load-bytecode fname)
  (let* ((f (open-input-file fname))
		 (raw-code (read f)))
	(close-port f)
	(for-each execute (map frame->compiled-function (reconstitute-code raw-code)))
	'OK))


(define (load-file fname)
  (cond ((string-suffix? ".scm" fname)
		 (load fname))
		((string-suffix? ".scmc" fname)
		 (load-bytecode fname))
		(else
		 (let* ((source-name (string-append fname ".scm"))
				(bytecode-name (string-append fname ".scmc"))
				(source-time (if (file-exists? source-name)
							   (file-modification-time source-name)
							   0))
				(bytecode-time (if (file-exists? bytecode-name)
								 (file-modification-time bytecode-name)
								 0)))
		   (if (> bytecode-time source-time)
			 (and (> bytecode-time 0)
				  (load-bytecode bytecode-name))
			 (and (> source-time 0)
				  (load source-name)))))))

;;;-----------------------------------------------------------------------------
;;; Compiling a file of source code


(define (expand-code code)
  (vector-map (lambda (c)
				(if (eqv? (vector-first c) 20) ; FN?
				  (let ((f (vector-second c)))
					(vector 20 (make-frame name: (compiled-name f)
										   env: (compiled-env f)
										   args: (compiled-args f)
										   code: (expand-code (compiled-code f)))))
				  c))
			  code))


(define (compiled-function->frame func)
  (make-frame name: (compiled-name func)
			  env: (compiled-env func)
			  args: (compiled-args func)
			  code: (expand-code (compiled-code func))))


(define (save-compiled-function fname functions)
  (let ((bytecode-file (open-output-file fname)))
	(format bytecode-file "~A" (map compiled-function->frame functions))
	(close-port bytecode-file)))


(define (compile-sexprs source-file result)
  (let ((sexpr (read source-file)))
	(if (eof-object? sexpr)
	  result
	  (compile-sexprs source-file (cons (compile sexpr) result)))))


(define (compile-file fname)
  ;; (format #t "Compiling ~A~%" fname)
  (let* ((source-name (if (string-suffix? ".scm" fname)
						fname
						(string-append fname ".scm"))))
	(if (file-exists? source-name)
	  (begin (format #t "Compiling: ~A~%" source-name)
		 	 (let* ((bytecode-name (string-append source-name "c"))
					(source-file (open-input-file source-name))
					(compiled-code (reverse (compile-sexprs source-file '()))))
			   (close-port source-file)
			   (save-compiled-function bytecode-name compiled-code)
			   (format #t "Compiled ~S~%Bytecode saved to ~S~%" source-name bytecode-name)))
	  (format #t "Could not find file: ~S~%" source-name))))


(define (compile-directory dirname)
  (when (file-exists? dirname)
	(for-each (lambda (fname)
				(cond ((file-directory? fname)
					   (compile-directory fname))
					  ((string-suffix? ".scm" fname)
					   (compile-file fname))))
			  (directory-read dirname))))

