;;; -*- mode: Scheme -*-

(load (string-join (list (get-env "GOLISPHOME") "tools" "compiler.scm") "/"))


(context "Optimizer"

		 ()

		 (it "removes unreferenced labels"
			 (assert-eq (optimize '((POP) L1 (RETURN))) '((POP) (RETURN))))


		 (it "simplifies gset-pop-gvar"
			 (assert-eq (optimize '((GSET a) (POP) (GVAR a)))
						'((GSET a))))

		 (it "simplifies lset-pop-lget"
			 (assert-eq (optimize '((LSET a) (POP) (LVAR a)))
						'((LSET a))))

		 (it "removes dead code between a JUMP and the next label"
			 (assert-eq (optimize '((JUMP L2) (JUMP L1) (ONE) L2 (RETURN)))
						'((JUMP L2) (JUMP L1) L2 (RETURN))))

		 (it "removes a trivial jump"
			 (assert-eq (optimize '((ONE) (JUMP L1) L1 (RETURN)))
						'((ONE) (RETURN))))

		 (it "retargets jumps to avoid hopping"
			 (assert-eq (optimize '((JUMP L1) (POP) L1 (JUMP L2)))
						'((JUMP L2) (JUMP L2))))
		 
		 (it "retargets on-true jumps to avoid hopping"
			 (assert-eq (optimize '((TJUMP L1) (POP) L1 (JUMP L2)))
						'((TJUMP L2) (POP) (JUMP L2))))

		 (it "retargets on-false jumps to avoid hopping"
			 (assert-eq (optimize '((FJUMP L1) (POP) L1 (JUMP L2)))
						'((FJUMP L2) (POP) (JUMP L2))))

		 (it "collapses (TRUE) (NOT) to (FALSE)"
			 (assert-eq (optimize '((TRUE) (NOT)))
						'((FALSE))))

		 (it "Removes TJUMPs that are never taken"
			 (assert-eq (optimize '((FALSE) (TJUMP L) (POP)))
						'((POP))))

		 (it "Makes an always taken TJUMP into a JUMP"
			 (assert-eq (optimize '((TRUE) (TJUMP L)))
						'((JUMP L))))

		 (it "collapses (ZERO) (NOT) to (FALSE)"
			 (assert-eq (optimize '((ZERO) (NOT)))
						'((FALSE))))

		 (it "Makes an always taken TJUMP into a JUMP"
			 (assert-eq (optimize '((ZERO) (TJUMP L)))
						'((JUMP L))))

		 (it "collapses (ONE) (NOT) to (FALSE)"
			 (assert-eq (optimize '((ONE) (NOT)))
						'((FALSE))))

		 (it "Makes an always taken TJUMP into a JUMP"
			 (assert-eq (optimize '((ONE) (TJUMP L)))
						'((JUMP L))))

		 (it "collapses (TWO) (NOT) to (FALSE)"
			 (assert-eq (optimize '((TWO) (NOT)))
						'((FALSE))))

		 (it "Makes an always taken TJUMP into a JUMP"
			 (assert-eq (optimize '((TWO) (TJUMP L)))
						'((JUMP L))))

		 (it "collapses (NIL) (NOT) to (FALSE)"
			 (assert-eq (optimize '((NIL) (NOT)))
						'((FALSE))))

		 (it "Makes an always taken TJUMP into a JUMP"
			 (assert-eq (optimize '((NIL) (TJUMP L)))
						'((JUMP L))))

		 (it "collapses (FALSE) (NOT) to (TRUE)"
			 (assert-eq (optimize '((FALSE) (NOT)))
						'((TRUE))))

		 (it "Removes FJUMPs that are never taken"
			 (assert-eq (optimize '((TRUE) (FJUMP L) (POP)))
						'((POP))))

		 (it "Makes an always taken FUMP into a JUMP"
			 (assert-eq (optimize '((FALSE) (FJUMP L)))
						'((JUMP L))))

		 )
