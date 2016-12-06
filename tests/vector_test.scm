;;; -*- mode: Scheme -*-

(context "An vector initilized with a size and no value"

         ((define v (make-vector 5)))

         (it "can be created"
             (assert-not-nil v))

         (it "is a vector"
             (assert-true (vector? v)))

         (it "has the right length"
             (assert-eq (vector-length v) 5))

         (it "has converts to a list of nils"
             (assert-eq (vector->list v) '(() () () () ())))

         (it "raises an error when given a non integer size"
             (assert-error (make-vector "hi"))))


(context "A vector initialized with a size and value"

         ((define v (make-vector 5 'a)))

         (it "can be created"
             (assert-not-nil v))

         (it "is a vector"
             (assert-true (vector? v)))

         (it "has the right length"
             (assert-eq (vector-length v) 5))

         (it "has converts to the correct list"
             (assert-eq (vector->list v) '(a a a a a)))

          (it "raises an error when given a non integer size"
              (assert-error (make-vector "hi"))))

(context "A vector initialized with values"

         ((define v (vector 'a 'b 'c 4 5)))

         (it "can be created"
             (assert-not-nil v))

         (it "is a vector"
             (assert-true (vector? v)))

         (it "has the right length"
             (assert-eq (vector-length v) 5))

         (it "has converts to the correct list"
             (assert-eq (vector->list v) '(a b c 4 5))))

(context "A literal vector"

         ((define v #(a b c 4 5)))

         (it "can be created"
             (assert-not-nil v))

         (it "is a vector"
             (assert-true (vector? v)))

         (it "has the right length"
             (assert-eq (vector-length v) 5))

         (it "has converts to the correct list"
             (assert-eq (vector->list v) '(a b c 4 5))))

(context "A vector"

         ((define v (vector 'a 'b 1 '(b d))))

         (it "supports direct access to elements"
             (assert-eq (vector-ref v 0) 'a)
             (assert-eq (vector-ref v 1) 'b)
             (assert-eq (vector-ref v 2) 1)
             (assert-eq (vector-ref v 3) '(b d)))

         (it "supports mutation of elements"
             (vector-set! v 1 "test")
             (assert-eq (vector-ref v 1) "test"))

         (it "support element mutation using generic set-nth!"
             (set-nth! 1 v "test")
             (assert-eq (vector-ref v 1) "test"))

         (it "raises an error when given a non vector"
             (assert-error (vector-set! '(a b) 1 0)))

         (it "raises an error when given a non integer index"
             (assert-error (vector-set! v "hi" 0)))

         (it "raises an error when given a non vector"
             (assert-error (vector-ref '(a) 0)))

         (it "raises an error when given a non integer index"
             (assert-error (vector-ref v "hi")))

         (it "raises an error when the index is out of bounds"
             (assert-error (vector-ref v 4))))

(context "Taking the length of a vector"

         ()

         (it "return the correct result"
             (assert-eq (vector-length '#(1 2 3)) 3)
             (assert-eq (vector-length '#()) 0))

         (it "raises an error when given a non vector"
             (assert-error (vector-length '(1 2 3)))))

(context "A long vector"

         ((define v (vector 1 2 3 4 5 6 7 8 9 10)))

         (it "supports access to the first element (element 0)"
             (assert-eq (vector-first v) 1))

         (it "supports access to the second element (element 1)"
             (assert-eq (vector-second v) 2))

         (it "supports access to the third element (element 2)"
             (assert-eq (vector-third v) 3))

         (it "supports access to the fourth element (element 3)"
             (assert-eq (vector-fourth v) 4))

         (it "supports access to the fifth element (element 4)"
             (assert-eq (vector-fifth v) 5))

         (it "supports access to the sixth element (element 5)"
             (assert-eq (vector-sixth v) 6))

         (it "supports access to the seventh element (element 6)"
             (assert-eq (vector-seventh v) 7))

         (it "supports access to the eighth element (element 7)"
             (assert-eq (vector-eighth v) 8))

         (it "supports access to the ninth element (element 8)"
             (assert-eq (vector-ninth v) 9))

         (it "supports access to the tenth element (element 9)"
             (assert-eq (vector-tenth v) 10)))

(context "An empty vector"

         ((define v '#()))

         (it "has a length of 0"
             (assert-eq (vector-length v) 0))

         (it "raise an error on any element access"
             (assert-error (vector-ref v 0))
             (assert-error (vector-first v))
             (assert-error (vector-second v))
             (assert-error (vector-third v))
             (assert-error (vector-fourth v))
             (assert-error (vector-fifth v))
             (assert-error (vector-sixth v))
             (assert-error (vector-seventh v))
             (assert-error (vector-eighth v))
             (assert-error (vector-ninth v))
             (assert-error (vector-tenth v))))

(context "List to vector conversion"

         ((define l '(1 2 3 4))
          (define v (list->vector l)))

         (it "converts a list to a vector"
             (assert-true (vector? v)))

         (it "converts to the correct vector"
             (assert-eq v '#(1 2 3 4)))

         (it "raises an error when given a non list"
             (assert-error (list->vector 3))))

(context "vector to list conversion"

         ((define v '#(1 2 3 4))
          (define l (vector->list v)))
         
         (it "converts a vector to a list"
             (assert-true (list? l)))

         (it "converts to the correct  list"
             (assert-eq l '(1 2 3 4)))

         (it "raises an error when given a non vector"
             (assert-error (vector->list 3))))

(context "A copied vector"

         ((define v #(1 2 3))
          (define v2 (vector-copy v)))

         (it "has the same contents"
             (assert-eq v2 v))

         (it "has a different underlying array"
             (vector-set! v2 2 "hi")
             (assert-neq v v2))

         (it "raises an error when given a non vector"
             (assert-error (vector-copy '(a b c)))))

(context "A vector created with an initialization procedure"

         ((define v (make-initialized-vector 5 (lambda (x) (* x x)))))

         (it "is a vector"
             (assert-true (vector? v)))

         (it "has the correct length"
             (assert-eq (vector-length v) 5))

         (it "has the correct contents"
             (assert-eq (vector-first v) 0)
             (assert-eq (vector-second v) 1)
             (assert-eq (vector-third v) 4)
             (assert-eq (vector-fourth v) 9)
             (assert-eq (vector-fifth v) 16))

         (it "raises an error when given a non integer size"
             (assert-error (make-initialized-vector 'a +)))

         (it "raises an error when given a non function for initialization"
             (assert-error (make-initialized-vector 5 'a))))

(context "A result of growing a vector"

         ((define v '#(1 2 3))
          (define v2 (vector-grow v 10)))

         (it "is a vector"
             (assert-true (vector? v2)))

         (it "has the correct length"
             (assert-eq (vector-length v2) 10))

         (it "has the correct contents"
             (assert-eq (vector-first v2) 1)
             (assert-eq (vector-second v2) 2)
             (assert-eq (vector-third v2) 3)
             (assert-eq (vector-fourth v2) '())
             (assert-eq (vector-fifth v2) '())
             (assert-eq (vector-sixth v2) '())
             (assert-eq (vector-seventh v2) '())
             (assert-eq (vector-eighth v2) '())
             (assert-eq (vector-ninth v2) '())
             (assert-eq (vector-tenth v2) '()))

         (it "raises an error when given a non vector"
             (assert-error (vector-grow 1 1)))

         (it "raises an error when given a non integer size"
             (assert-error (vector-grow v 'a)))

         (it "raises an error when given a new size not greater than the existing size"
             (assert-error (vector-grow v 2))))

(context "The result of mapping a vector"

         ((define v '#((a b) (d e) (g h)))
          (define result (vector-map cadr v)))

         (it "is a vector"
             (assert-true (vector? result)))
         
         (it "has the correct length"
             (assert-eq (vector-length result) 3))

         (it "has the correct values"
             (assert-eq (vector-first result) 'b)
             (assert-eq (vector-second result) 'e)
             (assert-eq (vector-third result) 'h))

         (it "generates an error when given a non function"
             (assert-error (vector-map 1 v)))

         (it "generates an error when given a non vectors"
             (assert-error (vector-map + '(a b c)))))

(context "The result of mapping over multiple vectors"

         ((define v1 '#(1 2 3 4))
          (define v2 '#(5 6 7 8))
          (define result (vector-map + v1 v2)))

         (it "is a vector"
             (assert-true (vector? result)))
         
         (it "has the correct length"
             (assert-eq (vector-length result) 4))

         (it "has the correct values"
             (assert-eq (vector-first result) 6)
             (assert-eq (vector-second result) 8)
             (assert-eq (vector-third result) 10)
             (assert-eq (vector-fourth result) 12))

         (it "generates an error when given a non function"
             (assert-error (vector-map 1 v1 v2)))

         (it "generates an error when given a non vectors"
             (assert-error (vector-map + '(a b c) '#(1 2 3)))))

(context "Iterating over a vector using vector-for-each"

         ((define count 0)
          (define result (vector-for-each (lambda (x) (set! count (+ count x))) '#(1 2 3 4))))
         
         (it "returns nil"
             (assert-eq result '()))
         
         (it "has the required side effect"
             (assert-eq count 10))

         (it "raises an error when given a non function"
             (assert-error (vector-for-each 5 '#())))

         (it "raises an error when given a non vector"
             (assert-error (vector-for-each zero? '()))))


(context "Reducing a vector"

         ()

         (it "returns initial value when given an empty vector"
             (assert-eq (vector-reduce + 0 '#()) 0))

         (it "returns first item when given a singleton vector"
             (assert-eq (vector-reduce + 0 '#(1)) 1))

         (it "returns correct value using a lambda"
             (assert-eq (vector-reduce (lambda (acc item) (+ acc item)) 0 '#(1 2 3)) 6))

         (it "returns correct value using a primitive"
             (assert-eq (vector-reduce + 0 '#(1 2 3)) 6))

         (it "raises an error is given a non function"
             (assert-error (vector-reduce 7 0 '#())))

         (it "raises an error if given a non-vector"
             (assert-error (vector-reduce + 0 '()))))


(context "Filtering a vector"

         ((define evens '#(0 2 4 6 8))
          (define odds '#(1 3 5 7 9))
          (define all '#(0 1 2 3 4 5 6 7 8 9)))

         (it "returns an empty vector when given an empty vector"
             (assert-eq (vector-filter even? '#()) '#()))

         (it "returns the correct value"
             (assert-eq (vector-filter even? all) evens)
             (assert-eq (vector-filter odd? all) odds))

         (it "returns an empty vector when nothing matches"
             (assert-eq (vector-filter even? odds) '#()))

         (it "raises an error when given a non funtion"
             (assert-error (vector-filter 5 all)))

         (it "raises an error when given a non-vector"
             (assert-error (vector-filter even? '()))))

(context "Removing from a vector"

         ((define evens '#(0 2 4 6 8))
          (define odds '#(1 3 5 7 9))
          (define all '#(0 1 2 3 4 5 6 7 8 9)))

         (it "returns an empty vector when given an empty vector"
             (assert-eq (vector-remove even? '#()) '#()))

         (it "returns the correct value"
             (assert-eq (vector-remove even? all) odds)
             (assert-eq (vector-remove odd? all) evens))

         (it "returns an empty vector when everything matches"
             (assert-eq (vector-remove even? evens) '#()))

         (it "raises an error when given a non funtion"
             (assert-error (vector-remove 5 all)))

         (it "raises an error when given a non-vector"
             (assert-error (vector-remove even? '()))))

(context "Subvector extraction"

         ((define v (vector 1 2 3 4 5 6 7 8 9 10)))

         (it "results in the correct vectors"
             (assert-eq (subvector v 1 3) '#(2 3))
             (assert-eq (subvector v 4 8) '#(5 6 7 8))
             (assert-eq (subvector v 3 3) '#()))

         (it "raises an error when given a non vector"
             (assert-error (subvector '(1 2 3 4) 1 2)))

         (it "raises an error when the starting index is negative"
             (assert-error (subvector v -1 5)))

         (it "raises an error when the starting index is beyond end of the vector"
             (assert-error (subvector v 20 24)))

         (it "raises an error when the ending index is less than the starting index"
             (assert-error (subvector v 4 2)))

         (it "raises an error when the ending index is beyond the end of the vector"
             (assert-error (subvector v 8 12))))

(context "Taking the head of a vector"

         ((define v '#(1 2 3 4 5 6 7 8 9 10))
          (define h (vector-head v 5)))

         (it "results in a vector"
             (assert-true (vector? h)))

         (it "has the correct length"
             (assert-eq (vector-length h) 5))

         (it "has the correct contents"
             (assert-eq h '#(1 2 3 4 5)))

         (it "can take a zero length head"
             (assert-eq (vector-head v 0) '#()))

         (it "raises an error when given a non vector argument"
             (assert-error (vector-head '(1 2 3) 0)))

         (it "raises an error when the ending index is negative"
             (assert-error (vector-head v -1)))

         (it "raises an error when the ending index is beyond end of the vector"
             (assert-error (vector-head v 20))))

(context "Taking the tail of a vector"

         ((define v '#(1 2 3 4 5 6 7 8 9 10))
          (define t (vector-tail v 5)))

         (it "results in a vector"
             (assert-true (vector? t)))

         (it "has the correct length"
             (assert-eq (vector-length t) 5))

         (it "has the correct contents"
             (assert-eq t '#(6 7 8 9 10)))

         (it "can take a zero length tail"
             (assert-eq (vector-tail v 10) '#()))

         (it "raises an error when given a non vector argument"
             (assert-error (vector-tail '(1 2 3) 0)))

         (it "raises an error when the starting index is negative"
             (assert-error (vector-tail v -1)))

         (it "raises an error when the starting index is beyond end of the vector"
             (assert-error (vector-tail v 20))))

(context "Filling a of vector"

         ((define v '#(1 2 3 4 5))
          (vector-fill! v "hi"))

         (it "is still a vector"
             (assert-true (vector? v)))

         (it "has the same length"
             (assert-eq (vector-length v) 5))

         (it "has updated contents"
             (assert-eq v '#("hi" "hi" "hi" "hi" "hi")))

         (it "raises an error when given a non vector"
             (assert-error (vector-fill! '(a b c) "hi"))))

(context "Filling a of subvector"

         ((define v '#(1 2 3 4 5))
          (subvector-fill! v 1 3 "hi"))

         (it "is still a vector"
             (assert-true (vector? v)))

         (it "has the same length"
             (assert-eq (vector-length v) 5))

         (it "has updated contents"
             (assert-eq v '#(1 "hi" "hi" 4 5)))

         (it "raises an error when given a non vector"
             (assert-error (subvector-fill! '(a b c) 1 2 "hi")))
         
         (it "raises an error when the starting index is negative"
             (assert-error (subvector-fill! v -1 2 "hi")))
         
         (it "raises an error when the starting index is beyond end of the vector"
             (assert-error (subvector-fill! v 20 23 "hi")))

         (it "raises an error when the ending index is less than the starting index"
             (assert-error (subvector-fill! v 4 2 "hi")))

         (it "raises an error when the ending index is beyond the end of the vector"
             (assert-error (subvector-fill! v 8 12 "hi"))))

(context "Copying a subvector from the left"

         ((define v '#(1 2 3 4 5 6 7 8 9 10))
          (define v2 '#(11 12 13 14 15 16 17 18 19 20)))

         (it "updates and returns the destination vector"
             (assert-eq (subvector-move-left! v 0 4 v2 3) '#(11 12 13 1 2 3 4 18 19 20))
             (assert-eq v2 '#(11 12 13 1 2 3 4 18 19 20)))

         (it "copies from the left (shown using a single vector)"
             (assert-eq (subvector-move-left! v 4 9 v 2) '#(1 2 5 6 7 8 9 8 9 10))
             (assert-eq v '#(1 2 5 6 7 8 9 8 9 10)))

         (it "raises an error when given a non vector for the source"
             (assert-error (subvector-move-left! '(a b c) 1 2 "hi")))
         
         (it "raises an error when the source starting index is negative"
             (assert-error (subvector-move-left! v -1 2 "hi")))
         
         (it "raises an error when the source starting index is beyond end of the source vector"
             (assert-error (subvector-move-left! v 20 23 "hi")))

         (it "raises an error when the source ending index is less than the source starting index"
             (assert-error (subvector-move-left! v 4 2 "hi")))

         (it "raises an error when the source ending index is beyond the end of the source vector"
             (assert-error (subvector-move-left! v 8 12 "hi")))

         (it "raises an error when given a non vector for the destination"
             (assert-error (subvector-move-left! '(a b c) 1 2 "hi")))
         
         (it "raises an error when the destination starting index is negative"
             (assert-error (subvector-move-left! v -1 2 "hi")))
         
         (it "raises an error when the destination starting index is beyond end of the destinationvector"
             (assert-error (subvector-move-left! v 20 23 "hi")))

         (it "raises an error when the source substring is longer than the space specified in the destination vector"
             (assert-error (subvector-move-left! v 0 8 v2 8)))

)
(context "Copying a subvector from the right"

         ((define v '#(1 2 3 4 5 6 7 8 9 10))
          (define v2 '#(11 12 13 14 15 16 17 18 19 20)))

         (it "updates and returns the destination vector"
             (assert-eq (subvector-move-right! v 0 4 v2 3) '#(11 12 13 1 2 3 4 18 19 20))
             (assert-eq v2 '#(11 12 13 1 2 3 4 18 19 20)))

         (it "copies from the right (shown using a single vector)"
             (assert-eq (subvector-move-right! v 2 5 v 4) '#(1 2 3 4 3 4 5 8 9 10))
             (assert-eq v '#(1 2 3 4 3 4 5 8 9 10)))

         (it "raises an error when given a non vector for the source"
             (assert-error (subvector-move-right! '(a b c) 1 2 "hi")))
         
         (it "raises an error when the source starting index is negative"
             (assert-error (subvector-move-right! v -1 2 "hi")))
         
         (it "raises an error when the source starting index is beyond end of the source vector"
             (assert-error (subvector-move-right! v 20 23 "hi")))

         (it "raises an error when the source ending index is less than the source starting index"
             (assert-error (subvector-move-right! v 4 2 "hi")))

         (it "raises an error when the source ending index is beyond the end of the source vector"
             (assert-error (subvector-move-right! v 8 12 "hi")))

         (it "raises an error when given a non vector for the destination"
             (assert-error (subvector-move-right! '(a b c) 1 2 "hi")))
         
         (it "raises an error when the destination starting index is negative"
             (assert-error (subvector-move-right! v -1 2 "hi")))
         
         (it "raises an error when the destination starting index is beyond end of the destinationvector"
             (assert-error (subvector-move-right! v 20 23 "hi")))

         (it "raises an error when the source substring is longer than the space specified in the destination vector"
             (assert-error (subvector-move-right! v 0 8 v2 8))))


(context "Sorting a vector"

         ((define unsorted '#(8 9 2 3 6 6 9 2 0 5 7))
          (define sorted (vector-sort unsorted <)))

         (it "returns a vector"
             (assert-true (vector? sorted)))

         (it "returns something with the right length"
             (assert-eq (vector-length sorted) 11))

         (it "returns a sorted result"
             (assert-eq sorted '#(0 2 2 3 5 6 6 7 8 9 9)))

         (it "doesn't affect the original"
             (assert-eq unsorted '#(8 9 2 3 6 6 9 2 0 5 7)))

         (it "raises an error when given a non vector"
             (assert-error (vector-sort '(1 2 3) <)))

         (it "raises an error if given a non procedure"
             (assert-error (vector-sort unsorted 5))))

(context "Sorting a vector in place"

         ((define unsorted '#(8 9 2 3 6 6 9 2 0 5 7))
          (define sorted (vector-sort! unsorted <)))

         (it "returns a vector"
             (assert-true (vector? sorted)))

         (it "returns something with the right length"
             (assert-eq (vector-length sorted) 11))

         (it "returns a sorted result"
             (assert-eq sorted '#(0 2 2 3 5 6 6 7 8 9 9)))

         (it "updates the original"
             (assert-eq unsorted sorted))
         
         (it "raises an error when given a non vector"
             (assert-error (vector-sort! '(1 2 3) <)))

         (it "raises an error if given a non procedure"
             (assert-error (vector-sort! unsorted 5))))

(context "Linear search of a vector using a predicate"

         ()
         
         (it "returns the item if present"
             (assert-eq (vector-find even? '#(3 1 4 1 5 9)) 4))

         (it "returns #f if not found"
             (assert-false (vector-find even? '#(1 3 5 7 9))))

         (it "raises an error if given a nonfunction"
             (assert-error (vector-find 5 '())))

         (it "raises an error if given a non predicate"
             (assert-error (vector-find + '(1 2))))

         (it "raises an error if given a non vector"
             (assert-error (vector-find even? 5))))

