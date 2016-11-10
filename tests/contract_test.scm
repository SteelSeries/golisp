;;; -*- mode: Scheme -*-

(define **USE_CONTRACTS** #t)
(load "lisp/contracts.scm")

(context "contracts"
		 ()

		 (it "handles a fulfilled requirement"
			 (assert-nerror ((lambda ()
							   (contract-let ((x 1))
											 (contract-requires (> x 0))
											 x)))))

		 (it "handles an unfulfilled requirement"
			 (assert-error ((lambda (x)
							  (contract-let ((x 0))
											(contract-requires (> x 0))
											x)))))

		 (it "handles a fulfilled assurance"
			 (assert-nerror ((lambda ()
							   (contract-let ()
											 (contract-ensures (eq? contract-result 0))
											 0)))))

		 (it "handles a unfulfilled assurance"
			 (assert-error ((lambda ()
							  (contract-let ()
											(contract-ensures (eq? contract-result 0))
											1))))))


