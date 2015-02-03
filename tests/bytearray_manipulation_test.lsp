(describe list-to-bytearray
	;; Bytes
	(== (list-to-bytearray '(1 2 3 4 5)) [1 2 3 4 5])
	(== (list-to-bytearray '(255 64 83 2)) [255 64 83 2])
	(== (list-to-bytearray (list)) [])

	;; Bytearrays
	(== (list-to-bytearray '([0 1 2] [3 4 5] [64 83 112])) [0 1 2 3 4 5 64 83 112])

	;; Mixed
	(== (list-to-bytearray '(0 [1 2] [3 4 5] 64 83 112)) [0 1 2 3 4 5 64 83 112])
)

(describe bytearray-to-list
	(== (bytearray-to-list []) (list))
	(== (bytearray-to-list [1 2 3 4 5]) '(1 2 3 4 5))
)

(describe replace-byte
	(== (replace-byte [1 2 3 4 5] 0 8) [8 2 3 4 5])
	(== (replace-byte [255 64 83 2] 3 112) [255 64 83 112])
	;; Make sure the original is not modified
	(begin 
		(define a [1 2 3 4 5])
		(replace-byte a 0 8)
		(== a [1 2 3 4 5])
	)
)

(describe replace-byte!
	(== (replace-byte! [1 2 3 4 5] 0 8) [8 2 3 4 5])
	;; The original should be what is modified
	(begin
		(define a [1 2 3 4 5])
		(replace-byte! a 0 8)
		(== a [8 2 3 4 5])
	)
)

(describe extract-byte
	(== (extract-byte [1 2 3 4 5] 0) 1)
	(== (extract-byte [1 2 3 4 5] 4) 5)
)

(describe append-bytes
	;; Byte or bytes
	(== (append-bytes [1 2 3 4 5] 6) [1 2 3 4 5 6])
	(== (append-bytes [1 2 3 4 5] 6 7 112) [1 2 3 4 5 6 7 112])
	;; The original should not be modified
	(begin
		(define a [1 2 3 4 5])
		(append-bytes a 6 7 112)
		(== a [1 2 3 4 5])
	)

	;; List of bytes
	(== (append-bytes [1 2 3 4 5] (list)) [1 2 3 4 5])
	(== (append-bytes [1 2 3 4 5] '(6)) [1 2 3 4 5 6])
	(== (append-bytes [1 2 3 4 5] '(6 7 112)) [1 2 3 4 5 6 7 112])
	;; The original should not be modified
	(begin
		(define a [1 2 3 4 5])
		(append-bytes a '(6))
		(== a [1 2 3 4 5])
	)

	;; Another bytearray
	(== (append-bytes [1 2 3 4 5] []) [1 2 3 4 5])
	(== (append-bytes [1 2 3 4 5] [6 7 8]) [1 2 3 4 5 6 7 8])
	;; The original should not be modified
	(begin
		(define a [1 2 3 4 5])
		(append-bytes a [6 7 8])
		(== a [1 2 3 4 5])
	)

	;; Multiple bytearrays
	(== (append-bytes [1 2 3 4 5] [] [] []) [1 2 3 4 5])
	(== (append-bytes [1 2 3 4 5] [6 7 8] [9 10 11] [83 112]) [1 2 3 4 5 6 7 8 9 10 11 83 112])
	;; The original should not be modified
	(begin
		(define a [1 2 3 4 5])
		(append-bytes a [6 7 8] [9 10 11] [83 112])
		(== a [1 2 3 4 5])
	)	
)

(describe append-bytes!
	;; Byte or bytes
	(== (append-bytes! [1 2 3 4 5] 6) [1 2 3 4 5 6])
	(== (append-bytes! [1 2 3 4 5] 6 7 112) [1 2 3 4 5 6 7 112])
	;; The original SHOULD be modified
	(begin
		(define a [1 2 3 4 5])
		(append-bytes! a 6 7 112)
		(== a [1 2 3 4 5 6 7 112])
	)

	;; List of bytes
	(== (append-bytes! [1 2 3 4 5] (list)) [1 2 3 4 5])
	(== (append-bytes! [1 2 3 4 5] '(6)) [1 2 3 4 5 6])
	(== (append-bytes! [1 2 3 4 5] '(6 7 112)) [1 2 3 4 5 6 7 112])
	;; The original SHOULD be modified
	(begin
		(define a [1 2 3 4 5])
		(append-bytes! a '(6))
		(== a [1 2 3 4 5 6])
	)

	;; Another bytearray
	(== (append-bytes! [1 2 3 4 5] []) [1 2 3 4 5])
	(== (append-bytes! [1 2 3 4 5] [6 7 8]) [1 2 3 4 5 6 7 8])
	;; The original SHOULD be modified
	(begin
		(define a [1 2 3 4 5])
		(append-bytes! a [6 7 8])
		(== a [1 2 3 4 5 6 7 8])
	)

	;; Multiple bytearrays
	(== (append-bytes! [1 2 3 4 5] [] [] []) [1 2 3 4 5])
	(== (append-bytes! [1 2 3 4 5] [6 7 8] [9 10 11] [83 112]) [1 2 3 4 5 6 7 8 9 10 11 83 112])
	;; The original SHOULD be modified
	(begin
		(define a [1 2 3 4 5])
		(append-bytes! a [6 7 8] [9 10 11] [83 112])
		(== a [1 2 3 4 5 6 7 8 9 10 11 83 112])
	)	
)

(describe take
	(== (take 0 [1 2 3 4 5]) [])
	(== (take 1 [1 2 3 4 5]) [1])
	(== (take 3 [1 2 3 4 5]) [1 2 3])
	(== (take 5 [1 2 3 4 5]) [1 2 3 4 5])
	(== (take 7 [1 2 3 4 5]) [1 2 3 4 5])
)

(describe drop
	(== (drop 0 [1 2 3 4 5]) [1 2 3 4 5])
	(== (drop 1 [1 2 3 4 5]) [2 3 4 5])
	(== (drop 3 [1 2 3 4 5]) [4 5])
	(== (drop 5 [1 2 3 4 5]) [])
	(== (drop 7 [1 2 3 4 5]) [])
)

(describe extract-bytes
	(== (extract-bytes [1 2 3 4 5] 0 0) [])
	(== (extract-bytes [1 2 3 4 5] 0 1) [1])
	(== (extract-bytes [1 2 3 4 5] 0 3) [1 2 3])
	(== (extract-bytes [1 2 3 4 5] 3 0) [])
	(== (extract-bytes [1 2 3 4 5] 3 1) [4])
	(== (extract-bytes [1 2 3 4 5] 3 2) [4 5])
)