;;; Generative testing for GoLisp
;;; This is an example of testing the sort function
;;; Copyright 2016 Dave Astels

(define (sort< x)
  (sort x <))

(define sample-list (gen/such-that gen/not-empty (gen/list gen/int)))

(define sort-same-size-prop
  (prop/for-all (l sample-list)
                (eqv? (length v) (length (sort< v)))))

(define sort-idempotent-prop
  (prop/for-all (l sample-list)
                (equal? (sort< v) (sort< (sort< v)))))

(define sort-first-less-than-last-prop
  (prop/for-all (l sample-list)
                (let ((s (sort< l)))
                  (<= (first s) (last s)))))

(define sort-first-is-min-prop
  (prop/for-all (l sample-list)
                (equal? (min l) (first (sort< l)))))

(define sort-props '(sort-same-size-prop
                     sort-idempotent-prop
                     sort-first-less-than-last-prop
                     sort-first-is-min-prop))
