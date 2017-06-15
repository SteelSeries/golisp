;;; -*- mode: Scheme -*-

;;; Copyright 2015 SteelSeries ApS. All rights reserved.
;;; Use of this source code is governed by a BSD-style
;;; license that can be found in the LICENSE file.

;;; Misc functions

(define true #t)
(define false #f)

(define false? not)

(define (boolean=? obj1 obj2)
    (or (and obj1 obj2)
        (and (not obj1) (not obj2))))

(define (boolean/and obj1 . other-objects)
    (apply and (cons obj1 other-objects)))

(define (boolean/or obj1 . other-objects)
    (apply or (cons obj1 other-objects)))

(define (symbol<? symbol1 symbol2)
    (string<? (symbol->string SYMBOL1)
              (symbol->string SYMBOL2)))

(define string->symbol intern)

;;;-----------------------------------------------------------------------------
;;; Logging

(define **LOGGING** #f)

(define (logging on/off)
  (set! **LOGGING** on/off))

(define (log-it format-string . objects)

  (when **LOGGING**
	(apply format (if (nil? objects)
					  (list #t format-string)
					  (cons* #t format-string objects)))
	(newline)))


