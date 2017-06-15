;;; -*- mode: Scheme -*-

(load (string-join (list (get-env "GOLISPHOME") "tools" "compiler.scm") "/"))

(define (opt c) (eval `(optimize ',c) ***COMPILER-ENVIRONMENT***))

(context "Optimizer"

		 ()

		 (it "removes unreferenced labels"
			 (assert-eq (opt '((POP) L1 (RETURN))) '((POP) (RETURN))))


		 (it "simplifies gset-pop-gvar"
			 (assert-eq (opt '((GSET a) (POP) (GVAR a)))
						'((GSET a))))

		 (it "simplifies lset-pop-lget"
			 (assert-eq (opt '((LSET a) (POP) (LVAR a)))
						'((LSET a))))

		 (it "removes dead code between a JUMP and the next label"
			 (assert-eq (opt '((JUMP L2) (JUMP L1) (ONE) L2 (RETURN)))
						'((JUMP L2) (JUMP L1) L2 (RETURN))))

		 (it "removes a trivial jump"
			 (assert-eq (opt '((ONE) (JUMP L1) L1 (RETURN)))
						'((ONE) (RETURN))))

		 (it "retargets jumps to avoid hopping"
			 (assert-eq (opt '((JUMP L1) (POP) L1 (JUMP L2)))
						'((JUMP L2) (JUMP L2))))
		 
		 (it "retargets on-true jumps to avoid hopping"
			 (assert-eq (opt '((TJUMP L1) (POP) L1 (JUMP L2)))
						'((TJUMP L2) (POP) (JUMP L2))))

		 (it "retargets on-false jumps to avoid hopping"
			 (assert-eq (opt '((FJUMP L1) (POP) L1 (JUMP L2)))
						'((FJUMP L2) (POP) (JUMP L2))))

		 (it "collapses (TRUE) (NOT) to (FALSE)"
			 (assert-eq (opt '((TRUE) (NOT)))
						'((FALSE))))

		 (it "Removes TJUMPs that are never taken"
			 (assert-eq (opt '((FALSE) (TJUMP L) (POP)))
						'((POP))))

		 (it "Makes an always taken TJUMP into a JUMP"
			 (assert-eq (opt '((TRUE) (TJUMP L)))
						'((JUMP L))))

		 (it "collapses (ZERO) (NOT) to (FALSE)"
			 (assert-eq (opt '((ZERO) (NOT)))
						'((FALSE))))

		 (it "Makes an always taken TJUMP into a JUMP"
			 (assert-eq (opt '((ZERO) (TJUMP L)))
						'((JUMP L))))

		 (it "collapses (ONE) (NOT) to (FALSE)"
			 (assert-eq (opt '((ONE) (NOT)))
						'((FALSE))))

		 (it "Makes an always taken TJUMP into a JUMP"
			 (assert-eq (opt '((ONE) (TJUMP L)))
						'((JUMP L))))

		 (it "collapses (TWO) (NOT) to (FALSE)"
			 (assert-eq (opt '((TWO) (NOT)))
						'((FALSE))))

		 (it "Makes an always taken TJUMP into a JUMP"
			 (assert-eq (opt '((TWO) (TJUMP L)))
						'((JUMP L))))

		 (it "collapses (NIL) (NOT) to (FALSE)"
			 (assert-eq (opt '((NIL) (NOT)))
						'((FALSE))))

		 (it "Makes an always taken TJUMP into a JUMP"
			 (assert-eq (opt '((NIL) (TJUMP L)))
						'((JUMP L))))

		 (it "collapses (FALSE) (NOT) to (TRUE)"
			 (assert-eq (opt '((FALSE) (NOT)))
						'((TRUE))))

		 (it "Removes FJUMPs that are never taken"
			 (assert-eq (opt '((TRUE) (FJUMP L) (POP)))
						'((POP))))

		 (it "Makes an always taken FUMP into a JUMP"
			 (assert-eq (opt '((FALSE) (FJUMP L)))
						'((JUMP L))))

		 )
