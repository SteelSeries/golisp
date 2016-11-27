;;; Generative testing for GoLisp
;;; This is an example of testing the sort function
;;; Copyright 2016 Dave Astels

;;; create a function that sorts it's input using the < function

(define (sort< x)
  (sort x <))


;;; Generate non-empty lists of integers as input data

(define sample-list (gen/such-that gen/not-empty (gen/list gen/int)))


;;; The length of a sorted list is the same as the length of the original

(define sort-same-size-prop
  (prop/for-all (l sample-list)
                (eqv? (length v) (length (sort< v)))))


;;; Sorting a sorted list doesn't change the order

(define sort-idempotent-prop
  (prop/for-all (l sample-list)
                (equal? (sort< v) (sort< (sort< v)))))


;;; the first element of a sorted list is less than the last element

(define sort-first-less-than-last-prop
  (prop/for-all (l sample-list)
                (let ((s (sort< l)))
                  (<= (first s) (last s)))))


;;; The first element of a sorted list is the smallest element of the original

(define sort-first-is-min-prop
  (prop/for-all (l sample-list)
                (equal? (min l) (first (sort< l)))))


(define sort-props '(sort-same-size-prop
                     sort-idempotent-prop
                     sort-first-less-than-last-prop
                     sort-first-is-min-prop))
