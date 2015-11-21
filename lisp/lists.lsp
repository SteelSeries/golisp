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
