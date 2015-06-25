;;; -*- mode: Scheme -*-

(describe list-to-bytearray
          ;; Bytes
          (assert-eq (list-to-bytearray '(1 2 3 4 5))
                     [1 2 3 4 5])
          (assert-eq (list-to-bytearray '(255 64 83 2))
                     [255 64 83 2])

          ;; Bytearrays
          (assert-eq (list-to-bytearray '([0 1 2] [3 4 5] [64 83 112]))
                     [0 1 2 3 4 5 64 83 112])

          ;; Mixed
          (assert-eq (list-to-bytearray '(0 [1 2] [3 4 5] 64 83 112))
                     [0 1 2 3 4 5 64 83 112]))

(describe bytearray-to-list
          (assert-eq (bytearray-to-list [])
                     (list))
          (assert-eq (bytearray-to-list [1 2 3 4 5])
                     '(1 2 3 4 5)))

(describe replace-byte
          (assert-eq (replace-byte [1 2 3 4 5] 0 8)
                     [8 2 3 4 5])
          (assert-eq (replace-byte [255 64 83 2] 3 112)
                     [255 64 83 112])
          ;; Make sure the original is not modified
          (begin 
            (define a [1 2 3 4 5])
            (replace-byte a 0 8)
            (assert-eq a
                       [1 2 3 4 5])))

(describe replace-byte!
          (assert-eq (replace-byte! [1 2 3 4 5] 0 8)
                     [8 2 3 4 5])
          ;; The original should be what is modified
          (begin
            (define a
              [1 2 3 4 5])
            (replace-byte! a 0 8)
            (assert-eq a
                       [8 2 3 4 5])))

(describe extract-byte
          (assert-eq (extract-byte [1 2 3 4 5] 0)
                     1)
          (assert-eq (extract-byte [1 2 3 4 5] 4)
                     5))

(describe append-bytes
          ;; Byte or bytes
          (assert-eq (append-bytes [1 2 3 4 5] 6)
                     [1 2 3 4 5 6])
          (assert-eq (append-bytes [1 2 3 4 5] 6 7 112)
                     [1 2 3 4 5 6 7 112])
          ;; The original should not be modified
          (begin
            (define a [1 2 3 4 5])
            (append-bytes a 6 7 112)
            (assert-eq a
                       [1 2 3 4 5]))

          ;; List of bytes
          (assert-eq (append-bytes [1 2 3 4 5] '(6))
                     [1 2 3 4 5 6])
          (assert-eq (append-bytes [1 2 3 4 5] '(6 7 112))
                     [1 2 3 4 5 6 7 112])
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
                       [1 2 3 4 5])))

(describe append-bytes!
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
                       [1 2 3 4 5 6 7 8 9 10 11 83 112])))

(describe take
          (assert-eq (take 0 [1 2 3 4 5])
                     [])
          (assert-eq (take 1 [1 2 3 4 5])
                     [1])
          (assert-eq (take 3 [1 2 3 4 5])
                     [1 2 3])
          (assert-eq (take 5 [1 2 3 4 5])
                     [1 2 3 4 5])
          (assert-eq (take 7 [1 2 3 4 5])
                     [1 2 3 4 5]))

(describe drop
          (assert-eq (drop 0 [1 2 3 4 5])
                     [1 2 3 4 5])
          (assert-eq (drop 1 [1 2 3 4 5])
                     [2 3 4 5])
          (assert-eq (drop 3 [1 2 3 4 5])
                     [4 5])
          (assert-eq (drop 5 [1 2 3 4 5])
                     [])
          (assert-eq (drop 7 [1 2 3 4 5])
                     []))

(describe extract-bytes
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
                     [4 5]))

(describe make-list
          (assert-eq (make-list 5)
                     '(() () () () ()))
          (assert-eq (make-list 5 1)
                     '(1 1 1 1 1))
          (assert-eq (make-list 3 'a)
                     '(a a a)))
