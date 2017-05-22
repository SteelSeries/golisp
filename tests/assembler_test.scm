;;; -*- mode: Scheme -*-

(load (string-join (list (get-env "GOLISPHOME") "tools" "compiler.scm") "/"))


(context "Assembler first pass"

		 ()

		 (it "can measure the length of empty code"
			 (assert-eq (first (asm-first-pass '()))
						0))

		 (it "can measure the length of short code"
			 (assert-eq (first (asm-first-pass '((a))))
						1))

		 (it "can measure the length of longer code"
			 (assert-eq (first (asm-first-pass '((a) (b) (c) (d))))
						4))

		 (it "finds no labels when there aren't any"
			 (assert-eq (second (asm-first-pass '()))
						'()))
		 
		 (it "finds 1 labels when there is 1"
			 (assert-eq (second (asm-first-pass '((a) l1 (b))))
						'((l1 . 1))))
		 
		 (it "finds 3 labels when there are 3"
			 (assert-eq (second (asm-first-pass '((a) L1 (b) L2 (c) L3 (d))))
						'((L3 . 3) (L2 . 2) (L1 . 1))))
		 
		 )

(context "Assembler second pass"

		 ()

		 (it "returns a vector"
			 (assert-eq (type-of (asm-second-pass '() 0 '())) 'vector))

		 (it "doesn't change things that don't use labels"
			 (assert-eq (asm-second-pass '((NIL) (RETURN)) 2 '())
						#((NIL) (RETURN))))

		 (it "replaces label references"
			 (assert-eq (asm-second-pass '((NIL) (JUMP L2) (FJUMP L3) (RETURN)) 4 '((L3 . 1) (L2 . 4)))
						#((NIL) (JUMP 4) (FJUMP 1) (RETURN))))
		 )

(context "bytecode converter"

		 ()

		 (it "converts instructions to vectors"
			 (assert-eq (type-of (vector-first (convert-to-bytecode #((ONE) (TWO) (PLUS) (RETURN)))))
						'vector))
		 
		 (it "converts simple instructions"
			 (assert-eq (convert-to-bytecode #((ONE) (TWO) (PLUS) (RETURN)))
						#(#(1) #(2) #(32) #(16)))))
