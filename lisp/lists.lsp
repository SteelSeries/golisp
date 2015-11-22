;;; -*- mode: Scheme -*-

;;; Copyright 2015 SteelSeries ApS. All rights reserved.
;;; Use of this source code is governed by a BSD-style
;;; license that can be found in the LICENSE file.

;;; Lists support library
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

(define (length+ object)
  (cond ((list? object)
         (length object))
        ((circular-list? object)
         #f)
        (else
         (error "length+ expected a proper or circular list"))))

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

(define (except-last-pair x)
  (cond ((null? x)
         (error "except-last-pair requires a non-empty list"))
        ((circular-list? x)
         (error "except-last-pair requires a non-circular list."))
        (else
         (let loop ((l x)
                    (result '()))
           (cond ((pair? (cdr l))
                  (loop (cdr l) (append! result (car l))))
                 (else
                  result))))))

(define (except-last-pair! x)
  (cond ((null? x)
         (error "except-last-pair requires a non-empty list"))
        ((circular-list? x)
         (error "except-last-pair requires a non-circular list."))
        ((and (or (list? x) (dotted-list? x))
              (pair? (cdr x)))
         (let loop ((l x)
                    (prev '())
                    (result x))
           (cond ((pair? (cdr l))
                  (loop (cdr l) l result))
                 (else
                  (set-cdr! prev '())
                  result))))
        (else
         '())))

(define (delq element l)
  (remove (lambda (x) (eq? x element)) l))

(define (delv element l)
  (remove (lambda (x) (eqv? x element)) l))

(define (delete element l)
  (remove (lambda (x) (equal? x element)) l))

(define (alist? x)
  (and (list? x)
       (every dotted-pair? x)))
