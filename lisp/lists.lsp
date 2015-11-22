;;; -*- mode: Scheme -*-

;;; Copyright 2015 SteelSeries ApS. All rights reserved.
;;; Use of this source code is governed by a BSD-style
;;; license that can be found in the LICENSE file.

;;; Lists support llibrary
;;; Adds the rich set of standard Scheme list functions.  Only the
;;; basic list functions are builtin

(define (iota count . rest)
    (let ((start (if (null? rest) 0 (car rest)))
          (step (if (null? (cdr rest)) 1 (cadr rest))))
      (map (lambda (i)
             (+ start (* i step)))
           (interval 0 (+ start count -1)))))

(define (subvector->list vect start end)
    (vector->list (subvector vect start end)))

(define (substring->list string start end)
    (string->list (substring string start end)))
(define (list-head l k)
  (cond ((list? l)
         (sublist l 0 k))
        (else
         (error "list-head requires a proper list"))))

(define (list-tail l k)
  (cond ((list? l)
         (let loop ((the-tail l)
                    (n k))
           (cond ((= n 0)
                  the-tail)
                 ((null? the-tail)
                  (error "list-tail requires an index <= the length of the list"))
                 (else
                  (loop (cdr the-tail) (- n 1))))))
        (else
         (error "list-tail require a proper list."))))

(define (list-ref l k)
  (nth k l))

(define (last-pair x)
  (cond ((null? x)
         (error "last-pair requires a non-empty list"))
        ((circular-list? x)
         (error "last-pair requires a non-circular list"))
        (else
         (let loop ((l x))
           (cond ((pair? (cdr l))
                  (loop (cdr l)))
                 (else
                  l))))))

