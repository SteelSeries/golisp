
;;; -*- mode: Scheme -*-

(load (string-join (list (get-env "GOLISPHOME") "tools" "compiler.scm") "/"))

(context "Compiler"

		 ()

		 (it "can compile a simple expression using immediate constants and prims"
			 (assert-eq (compiled-code (compile '(+ 1 2)))
						#(#(18 0) #(1) #(2) #(32) #(16))))

		 (it "can compile a simple expression using literal constants"
			 (assert-eq (compiled-code (compile '(+ 10 20)))
						#(#(18 0) #(11 10) #(11 20) #(32) #(16))))

		 (it "can compile a simple expression using primitive functions"
			 (assert-eq (compiled-code (compile '(+ 1 1 1)))
						#(#(18 0) #(1) #(1) #(1) #(21 + 3) #(16))))

		 (it "***can compile a let construct (which involves a macro)"
			 (let* ((code (compiled-code (compile '(let ((a 1) (b 2)) (+ a b))))))
			   (assert-eq (vector-head code 3)
						  #(#(18 0) #(1) #(2)))
			   ;; The FN instruction
			   (assert-eq (vector-first (vector-ref code 3))
						  22)
			   ;; This is the embedded lambda resulting from the expansion of the let macto
			   (assert-eq (compiled-code (vector-second (vector-ref code 3)))
						  #(#(18 2) #(6 0 0) #(6 0 1) #(32) #(16)))
			   (assert-eq (vector-tail code 4)
						  #(#(17 2)))))

		 (it "compiles a map call"
			 (assert-eq (compiled-code (compile '(map + '(1 2 3) '(4 5 6))))
						#(#(18 0) #(8 +) #(11 (1 2 3)) #(11 (4 5 6)) #(21 map 3) #(16))))

		 (it "compiles a call to an interpreted function"
			 (assert-eq (compiled-code (compile '(head '(1 2 3))))
						#(#(18 0) #(11 (1 2 3)) #(22 head 1) #(16))))

		 (it "can compile function definition"
			 (let* ((code (compiled-code (compile '(define (add x y) (+ x y))))))
			   (assert-eq (vector-first code)
						  #(18 0))
			   ;; The FN instruction
			   (assert-eq (vector-first (vector-second code))
						  20)
			   ;; This is the embedded lambda resulting from the expansion of the let macro
			   (assert-eq (compiled-code (vector-second (vector-second code)))
						  #(#(18 2) #(6 0 0) #(6 0 1) #(32) #(16)))
			   (assert-eq (vector-tail code 2)
						  #(#(9 add) #(11 add) #(49) #(16)))))

		 
		 )
