;;; GoLisp bytecode assembler
;;;
;;; Implemented using function for the various bytecodes

(define (LABEL symbol)
  )

(define (CALL f arity)
  (emit (left-shift arity 8))
  (emit-pointer f))

(define (CONSTANT# val)
  (cond ((integer? val)
         (if (and (< val 127) (> val -127))
             (emit (+ (left-shift 0x10 8) val))
             (assembler-error (str "Immediate integer constant has to be between -127 and 127, but was given " val))))
        ((boolean? val)
         (emit (if val 0x1101 0x1100)))
        (else (assembly-error (str "Immediate constant can only be integer or boolean, but was given " val)))))

(define (CONSTANT val)
  (emit 0x1800)
  (emit (address-of val)))

(define (VARREF symbol)
  (emit 0x2000)
  (emit (address-of symbol)))

(define (RETURN)
  (emit 0x3000))

(define (BRA label)
  (let ((target (program-offset-of label))
        (short-difference (- label (+ 1 current-assembler-offset)))
        (long-difference (- label (+ 5 current-assembler-offset))))
    (cond ((< (abs short-difference))))))

(define (BRAF label))

(define (BRAT label))


(define (constant-size c)
  (cond ((boolean? c) 1)
        ((integer? c) (if (< (abs c) 128) 1 5))
        (else 5)))

;;; (OPCODE arg...)
;;; ->
;;; ((OPCODE arg...) address size (word...))

(define (pass-1 code)
  (let ((pc 0))
    (map (lambda (instruction)
           (let ((size (case (car instruction)
                         ((CALL) 5)
                         ((CONSTANT CONSTANT#) (constant-size (cadr instruction)))
                         ((VAR) 5)
                         ((RET) 1)
                         ((BRA BRAF BRAT) '(1 2)))))
             
             (list instruction size)))
         code)))


