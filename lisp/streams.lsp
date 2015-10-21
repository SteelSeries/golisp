;;; -*- mode: Scheme -*-

;;; Streams from SICP

(define (memo-proc proc)
  (let ((already-run? #f)
        (result #f))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? #t)
                 result)
          result))))

(defmacro (delay **expr**)
  `(memo-proc (lambda ()
                ,**expr**)))

(define (force p)
  (p))

(defmacro (stream-cons **a** **b**)
  `(cons ,**a** (delay ,**b**)))

(define (stream-car s)
  (car s))

(define (stream-cdr s)
  (force (cdr s)))

(define (stream-null? s)
  (null? s))

(define the-empty-stream '())

;;; stream-ref returns the nth element of a stream (where the first
;;; element of the stream is counted as the 0th)

(define (stream-ref s n)
  (cond ((stream-null? s) the-empty-stream)
        ((eq? n 0) (stream-car s))
        (else (stream-ref (stream-cdr s) (- n 1)))))

;;; filter a stream by pred

(define (stream-filter pred stream)
  (cond ((stream-null? stream)
         the-empty-stream)
        ((pred (stream-car stream))
         (stream-cons (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else
         (stream-filter pred (stream-cdr stream)))))

;;; stream-map maps a procedure onto a stream

(define (stream-map proc . streams)
  (if (stream-null? (car streams))
      the-empty-stream
      (stream-cons (apply proc (map stream-car streams))
                   (apply stream-map (cons proc (map stream-cdr streams))))))


;;; stream-for-each applies a procedure to each element of a 
;;; stream, but does not build the answers back up into a stream

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))


;;; can use stream-for-each to write a procedure for viewing
;;; streams
;;;
;;; NOTE: do not try this on infinite streams

(define (display-stream s)
  (stream-for-each write-line s))

;;; defining some finite streams

(define a (stream-cons 1 (stream-cons 2 the-empty-stream)))

(define b (stream-cons 3 (stream-cons 4 (stream-cons 5 the-empty-stream))))


;;; a procedure to help us define a finite stream with integers
;;; between a given interval

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (stream-cons low
                   (stream-enumerate-interval (+ low 1) high))))

(define stream-1-to-1000 (stream-enumerate-interval 1 1000))

(define (print-stream s n)
  (cond ((stream-null? s) the-empty-stream)
	((eq? n 0) 'done)
	(else (write-line (stream-car s))
	      (print-stream (stream-cdr s) (- n 1)))))
    

;;; defining add-streams

(define (add-streams s1 s2)
  (if (or (stream-null? s1) (stream-null? s2))
      the-empty-stream
      (stream-cons (+ (stream-car s1) (stream-car s2))
                   (add-streams (stream-cdr s1)
                                (stream-cdr s2)))))


;;; defining scale-stream

(define (scale-stream s factor)
  (stream-cons (* (stream-car s) factor)
	       (scale-stream (stream-cdr s) factor)))

;;; defining a few infinite streams

(define zeros (stream-cons 0 zeros))

(define ones (stream-cons 1 ones))

(define twos (stream-cons 2 twos))

(define odds (stream-cons 1 (add-streams odds twos)))

;;; defining integers, a stream of positive integers, two 
;;; different ways

(define integers
  (stream-cons 1 (add-streams integers ones)))

(define integers
  (add-streams (stream-cons 0 integers)
               ones))

;;; defining stream of fibonnaci numbers

(define fib
  (stream-cons 1
               (stream-cons 1
                            (add-streams fib
                                         (stream-cdr fib)))))

;;; partial-sums takes a stream as an argument and returns the 
;;; stream whose elements are s0, s0+s1, s0+s1+s2, s0+s1+s2+s3, ...
;;; for example, (partial-sum integers) would be 1, 3, 6, 10, 15, ...

(define (partial-sums s)
  (stream-cons (stream-car s)
               (add-streams (partial-sums s)
                            (stream-cdr s))))


;;; merge combines two ordered streams into one ordered result
;;; stream, eliminating repetitions

(define (merge s1 s2)
  (cond ((stream-null? s1)
         s2)
        ((stream-null? s2)
         s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (stream-cons s1car 
                               (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (stream-cons s2car
                               (merge s1 (stream-cdr s2))))
                 (else
                  (stream-cons s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

;;; A famous problem, first raised by R. Hamming, is to enumerate, 
;;; in ascending order with no repetitions, all positive integers
;;; with no prime factors other than 2, 3, or 5.  One obvious way 
;;; to do this is to simply test each integer in turn to see whether
;;; it has any factors other than 2, 3, and 5.  But this is very 
;;; inefficient, since, as the integers get larger, fewer and fewer
;;; of them fit the requirement.  As an alternative, let us call
;;; the required stream of numbers H and notice the following facts
;;; about it.

;;; h begins with 1
;;; The elements of (scale-stream h 2) are also elements of s.
;;; The same is true for (scale-stream h 3) and (scale-stream h 5)
;;; These are all the elements of h.

(define h
  (stream-cons 1
               (merge (scale-stream h 2)
                      (merge (scale-stream h 3)
                             (scale-stream h 5)))))
