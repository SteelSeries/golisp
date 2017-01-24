;;; -*- mode: Scheme -*-

(context "The bytearray system"

         ()
         
         (it "list-to-bytearray"
                   ;; Bytes
                   (assert-eq (list->bytearray '(1 2 3 4 5)) [1 2 3 4 5])
                   (assert-eq (list->bytearray '(255 64 83 2)) [255 64 83 2])

                   ;; Bytearrays
                   (assert-eq (list->bytearray '([0 1 2] [3 4 5] [64 83 112])) [0 1 2 3 4 5 64 83 112])

                   ;; Mixed
                   (assert-eq (list->bytearray '(0 [1 2] [3 4 5] 64 83 112)) [0 1 2 3 4 5 64 83 112])

                   (assert-error (list->bytearray nil))
                   (assert-error (list->bytearray 1))
                   (assert-error (list->bytearray 'a))
                   (assert-error (list->bytearray '(_____a)))
                   (assert-error (list->bytearray '(1 2 300)))
                   (assert-error (list->bytearray '(1 2 -2))))

         (it "bytearray->list"
                   (assert-eq (bytearray->list []) (list))
                   (assert-eq (bytearray->list [1 2 3 4 5]) '(1 2 3 4 5))

                   (assert-error (bytearray->list 'a))
                   (assert-error (bytearray->list '(1 2 3)))
                   (assert-error (bytearray->list nil)))

         (it "replace-byte"
                   (assert-eq (replace-byte [1 2 3 4 5] 0 8) [8 2 3 4 5])
                   (assert-eq (replace-byte [255 64 83 2] 3 112) [255 64 83 112])
                   ;; Make sure the original is not modified
                   (begin 
                     (define a [1 2 3 4 5])
                     (replace-byte a 0 8)
                     (assert-eq a [1 2 3 4 5]))

                   (assert-error (replace-byte 'a 0 0)) ;not a byte array
                   (assert-error (replace-byte [1 2 3 4 5] "0" 8)) ;index not an integer
                   (assert-error (replace-byte [1 2 3 4 5] 8 8)) ;index > length
                   (assert-error (replace-byte [1 2 3 4 5] -1 8)) ;index < 0
                   (assert-error (replace-byte [1 2 3 4 5] 0 3.2)) ;value not an integer
                   (assert-error (replace-byte [1 2 3 4 5] 0 300)) ;value not a byte
                   (assert-error (replace-byte [1 2 3 4 5] 0 -3))) ;value not a byte

         (it "replace-byte!"
                   (assert-eq (replace-byte! [1 2 3 4 5] 0 8) [8 2 3 4 5])
                   ;; The original should be what is modified
                   (begin
                     (define a
                       [1 2 3 4 5])
                     (replace-byte! a 0 8)
                     (assert-eq a [8 2 3 4 5]))

                   (assert-error (replace-byte 'a 0 0)) ;not a byte array
                   (assert-error (replace-byte! [1 2 3 4 5] "0" 8)) ;index not an integer
                   (assert-error (replace-byte! [1 2 3 4 5] 8 8)) ;index > length
                   (assert-error (replace-byte! [1 2 3 4 5] -1 8)) ;index < 0
                   (assert-error (replace-byte! [1 2 3 4 5] 0 3.2)) ;value not an integer
                   (assert-error (replace-byte! [1 2 3 4 5] 0 300)) ;value not a byte
                   (assert-error (replace-byte! [1 2 3 4 5] 0 -3))) ;value not a byte

         (it "extract-byte"
                   (assert-eq (extract-byte [1 2 3 4 5] 0) 1)
                   (assert-eq (extract-byte [1 2 3 4 5] 4) 5)

                   (assert-error (extract-byte 'a 0)) ;not a byte array
                   (assert-error (extract-byte [1 2 3 4 5] "0")) ;index not an integer
                   (assert-error (extract-byte [1 2 3 4 5] 8)) ;index > length
                   (assert-error (extract-byte [1 2 3 4 5] -2)) ;index < 0
                   )

         (it "append-bytes"
                   ;; Byte or bytes
                   (assert-eq (append-bytes [1 2 3 4 5] 6) [1 2 3 4 5 6])
                   (assert-eq (append-bytes [1 2 3 4 5] 6 7 112) [1 2 3 4 5 6 7 112])
                   ;; The original should not be modified
                   (begin
                     (define a [1 2 3 4 5]) (append-bytes a 6 7 112)
                     (assert-eq a [1 2 3 4 5]))

                   ;; List of bytes
                   (assert-eq (append-bytes [1 2 3 4 5] '(6)) [1 2 3 4 5 6])
                   (assert-eq (append-bytes [1 2 3 4 5] '(6 7 112)) [1 2 3 4 5 6 7 112])
                   ;; The original should not be modified
                   (begin
                     (define a [1 2 3 4 5])
                     (append-bytes a '(6))
                     (assert-eq a
                                [1 2 3 4 5]))

                   ;; Another bytearray
                   (assert-eq (append-bytes [1 2 3 4 5] [])
                              [1 2 3 4 5])
                   (assert-eq (append-bytes [1 2 3 4 5] [6 7 8])
                              [1 2 3 4 5 6 7 8])
                   ;; The original should not be modified
                   (begin
                     (define a [1 2 3 4 5])
                     (append-bytes a [6 7 8])
                     (assert-eq a
                                [1 2 3 4 5]))

                   ;; Multiple bytearrays
                   (assert-eq (append-bytes [1 2 3 4 5] [] [] [])
                              [1 2 3 4 5])
                   (assert-eq (append-bytes [1 2 3 4 5] [6 7 8] [9 10 11] [83 112])
                              [1 2 3 4 5 6 7 8 9 10 11 83 112])
                   ;; The original should not be modified
                   (begin
                     (define a [1 2 3 4 5])
                     (append-bytes a [6 7 8] [9 10 11] [83 112])
                     (assert-eq a
                                [1 2 3 4 5]))

                   (assert-error (append-bytes 'a 1)) ;1st arg must be a bytearray
                   (assert-error (append-bytes [1 2 3] 300)) ;non-byte
                   (assert-error (append-bytes [1 2 3] -5)) ;non-byte
                   (assert-error (append-bytes [1 2 3] 3.5)) ;non-int
                   (assert-error (append-bytes [1 2 3] "a")) ;non-int
                   (assert-error (append-bytes [1 2 3] '("a"))) ;non-byte in list
                   (assert-error (append-bytes [1 2 3] '(355))) ;non-byte in list
                   (assert-error (append-bytes [1 2 3] '(-3)))) ;non-byte in list

         (it "append-bytes!"
                   ;; Byte or bytes
                   (assert-eq (append-bytes! [1 2 3 4 5] 6)
                              [1 2 3 4 5 6])
                   (assert-eq (append-bytes! [1 2 3 4 5] 6 7 112)
                              [1 2 3 4 5 6 7 112])
                   ;; The original SHOULD be modified
                   (begin
                     (define a [1 2 3 4 5])
                     (append-bytes! a 6 7 112)
                     (assert-eq a
                                [1 2 3 4 5 6 7 112]))

                   ;; List of bytes
                   (assert-eq (append-bytes! [1 2 3 4 5] '(6))
                              [1 2 3 4 5 6])
                   (assert-eq (append-bytes! [1 2 3 4 5] '(6 7 112))
                              [1 2 3 4 5 6 7 112])
                   ;; The original SHOULD be modified
                   (begin
                     (define a [1 2 3 4 5])
                     (append-bytes! a '(6))
                     (assert-eq a
                                [1 2 3 4 5 6]))

                   ;; Another bytearray
                   (assert-eq (append-bytes! [1 2 3 4 5] [])
                              [1 2 3 4 5])
                   (assert-eq (append-bytes! [1 2 3 4 5] [6 7 8])
                              [1 2 3 4 5 6 7 8])
                   ;; The original SHOULD be modified
                   (begin
                     (define a [1 2 3 4 5])
                     (append-bytes! a [6 7 8])
                     (assert-eq a
                                [1 2 3 4 5 6 7 8]))

                   ;; Multiple bytearrays
                   (assert-eq (append-bytes! [1 2 3 4 5] [] [] [])
                              [1 2 3 4 5])
                   (assert-eq (append-bytes! [1 2 3 4 5] [6 7 8] [9 10 11] [83 112])
                              [1 2 3 4 5 6 7 8 9 10 11 83 112])
                   ;; The original SHOULD be modified
                   (begin
                     (define a [1 2 3 4 5])
                     (append-bytes! a [6 7 8] [9 10 11] [83 112])
                     (assert-eq a
                                [1 2 3 4 5 6 7 8 9 10 11 83 112]))

                   (assert-error (append-bytes! 'a 1)) ;1st arg must be a bytearray
                   (assert-error (append-bytes! [1 2 3] 300)) ;non-byte
                   (assert-error (append-bytes! [1 2 3] -5)) ;non-byte
                   (assert-error (append-bytes! [1 2 3] 3.5)) ;non-int
                   (assert-error (append-bytes! [1 2 3] "a")) ;non-int
                   (assert-error (append-bytes! [1 2 3] '("a"))) ;non-byte in list
                   (assert-error (append-bytes! [1 2 3] '(355))) ;non-byte in list
                   (assert-error (append-bytes! [1 2 3] '(-3)))) ;non-byte in list

         (it take
                   (assert-eq (take-bytes 0 [1 2 3 4 5])
                              [])
                   (assert-eq (take-bytes 1 [1 2 3 4 5])
                              [1])
                   (assert-eq (take-bytes 3 [1 2 3 4 5])
                              [1 2 3])
                   (assert-eq (take-bytes 5 [1 2 3 4 5])
                              [1 2 3 4 5])
                   (assert-eq (take-bytes 7 [1 2 3 4 5])
                              [1 2 3 4 5]))

         (it "  drop"
                   (assert-eq (drop-bytes 0 [1 2 3 4 5])
                              [1 2 3 4 5])
                   (assert-eq (drop-bytes 1 [1 2 3 4 5])
                              [2 3 4 5])
                   (assert-eq (drop-bytes 3 [1 2 3 4 5])
                              [4 5])
                   (assert-eq (drop-bytes 5 [1 2 3 4 5])
                              [])
                   (assert-eq (drop-bytes 7 [1 2 3 4 5])
                              []))

         (it "extract-bytes"
                   (assert-eq (extract-bytes [1 2 3 4 5] 0 0)
                              [])
                   (assert-eq (extract-bytes [1 2 3 4 5] 0 1)
                              [1])
                   (assert-eq (extract-bytes [1 2 3 4 5] 0 3)
                              [1 2 3])
                   (assert-eq (extract-bytes [1 2 3 4 5] 3 0)
                              [])
                   (assert-eq (extract-bytes [1 2 3 4 5] 3 1)
                              [4])
                   (assert-eq (extract-bytes [1 2 3 4 5] 3 2)
                              [4 5])

                   (assert-error (extract-bytes 'a 1)) ;1st arg must be a bytearray
                   (assert-error (extract-bytes [1 2 3 4 5] 10 1)) ;index too big
                   (assert-error (extract-bytes [1 2 3 4 5] -1 1)) ;negative index
                   (assert-error (extract-bytes [1 2 3 4 5] 3 10)) ;count too big
                   (assert-error (extract-bytes [1 2 3 4 5] 3 -1)) ;negative count
                   )

         (it "make-list"
                   (assert-eq (make-list 5)
                              '(() () () () ()))
                   (assert-eq (make-list 5 1)
                              '(1 1 1 1 1))
                   (assert-eq (make-list 3 'a)
                              '(a a a))))
