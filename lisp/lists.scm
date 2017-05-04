;;; -*- mode: Scheme -*-

;;; Copyright 2015 SteelSeries ApS. All rights reserved.
;;; Use of this source code is governed by a BSD-style
;;; license that can be found in the LICENSE file.

;;; Lists support library
;;; Adds the rich set of standard Scheme list functions.  Only the
;;; basic list functions are builtin

(define (iota count . args)
    (let ((start (if (null? args) 0 (car args)))
          (step (if (null? (cdr args)) 1 (cadr args))))
      (map (lambda (i)
             (+ start (* i step)))
           (interval 0 (+ start count -1)))))

(define (subvector->list vect start end)
    (vector->list (subvector vect start end)))

(define (substring->list string start end)
    (string->list (substring string start end)))

(define (length+ object)
  (cond ((list? object)
         (length object))
        ((circular-list? object)
         #f)
        (else
         (error "length+ expected a proper or circular list."))))

(define (list-head l k)
  (cond ((list? l)
         (sublist l 0 k))
        (else
         (error "list-head requires a proper list."))))

(define (list-tail l k)
  (cond ((list? l)
         (cond ((zero? k)
				l)
			   ((nil? l)
				(error "list-tail requires an index <= the length of the list."))
			   (else
				(list-tail (cdr l) (-1+ k)))))
        (else
         (error "list-tail require a proper list."))))

(define (list-ref l k)
  (nth k l))

(define (except-last-pair-1 l result)
  (cond ((pair? (cdr l))
		 (except-last-pair-1 (cdr l) (append result (list (car l)))))
		(else
		 result)))

(define (except-last-pair l)
  (cond ((nil? l)
         (error "except-last-pair requires a non-empty list."))
        ((circular-list? l)
         (error "except-last-pair requires a non-circular list."))
        (else
		 (except-last-pair-1 l '()))))

(define (except-last-pair-1! l prev result)
  (if (pair? (cdr l))
	  (except-last-pair-1! (cdr l) l result)
	  (begin
		(set-cdr! prev '())
		result)))


(define (except-last-pair! l)
  (cond ((null? l)
         (error "except-last-pair requires a non-empty list."))
        ((circular-list? l)
         (error "except-last-pair requires a non-circular list."))
        ((and (or (list? l) (dotted-list? l))
			  (pair? (cdr l)))
         (except-last-pair-1! l '() l))
		(else '())))

(define (take n l)
  (define (take-iter acc n l)
    (if (zero? n)
        acc
        (take-iter (cons (car l) acc) (-1+ n) (cdr l))))
  (reverse (take-iter nil n l)))

(define (drop n l)
  (if (zero? n)
      l
      (drop (-1+ n) (cdr l))))

(define (delq element l)
  (remove (lambda (x) (eq? x element)) l))

(define (delv element l)
  (remove (lambda (x) (eqv? x element)) l))

(define (delete element l)
  (remove (lambda (x) (equal? x element)) l))

(define (alist? x)
  (and (list? x)
       (every dotted-pair? x)))

(define del-assq dissq)
(define del-assv dissv)
(define del-assoc dissoc)

(define (head l)
  (car l))

(define (rest l)
  (cdr l))

(define (tail l)
  (cdr l))

(define (caar l)
  (general-car-cdr l #b111))

(define (cadr l)
  (general-car-cdr l #b110))

(define (cdar l)
  (general-car-cdr l #b101))

(define (cddr l)
  (general-car-cdr l #b100))

(define (caaar l)
  (general-car-cdr l #b1111))

(define (caadr l)
  (general-car-cdr l #b1110))

(define (cadar l)
  (general-car-cdr l #b1101))

(define (caddr l)
  (general-car-cdr l #b1100))

(define (cdaar l)
  (general-car-cdr l #b1011))

(define (cdadr l)
  (general-car-cdr l #b1010))

(define (cddar l)
  (general-car-cdr l #b1001))

(define (cdddr l)
  (general-car-cdr l #b1000))

(define (caaaar l)
  (general-car-cdr l #b11111))

(define (caaadr l)
  (general-car-cdr l #b11110))

(define (caadar l)
  (general-car-cdr l #b11101))

(define (caaddr l)
  (general-car-cdr l #b11100))

(define (cadaar l)
  (general-car-cdr l #b11011))

(define (cadadr l)
  (general-car-cdr l #b11010))

(define (caddar l)
  (general-car-cdr l #b11001))

(define (cadddr l)
  (general-car-cdr l #b11000))

(define (cdaaar l)
  (general-car-cdr l #b10111))

(define (cdaadr l)
  (general-car-cdr l #b10110))

(define (cdadar l)
  (general-car-cdr l #b10101))

(define (cdaddr l)
  (general-car-cdr l #b10100))

(define (cddaar l)
  (general-car-cdr l #b10011))

(define (cddadr l)
  (general-car-cdr l #b10010))

(define (cdddar l)
  (general-car-cdr l #b10001))

(define (cddddr l)
  (general-car-cdr l #b10000))

(define (first l)
  (general-car-cdr l #b11))

(define (second l)
  (general-car-cdr l #b110))

(define (third l)
  (general-car-cdr l #b1100))

(define (fourth l)
  (general-car-cdr l #b11000))

(define (fifth l)
  (general-car-cdr l #b110000))

(define (sixth l)
  (general-car-cdr l #b1100000))

(define (seventh l)
  (general-car-cdr l #b11000000))

(define (eighth l)
  (general-car-cdr l #b110000000))

(define (ninth l)
  (general-car-cdr l #b1100000000))

(define (tenth l)
  (general-car-cdr l #b11000000000))
