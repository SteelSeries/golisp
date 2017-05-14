;;; -*- mode: Scheme -*-

;;; Copyright 2015 SteelSeries ApS. All rights reserved.
;;; Use of this source code is governed by a BSD-style
;;; license that can be found in the LICENSE file.

;;; GoLisp compiler - utilities

(define (set-first! l v)
  (set-car! l v))

(define (set-rest! l v)
  (set-cdr! l v))

(define (set-second! l v)
  (set-car! (cdr l) v))

(define (rest l)
  (cdr l))

(define (rest2 l)
  (cddr l))

(define (rest3 l)
  (cdddr l))

(define (label? x)
  (symbol? x))

(define (opcode instr)
  (if (label? instr)
	  'LABEL
	  (car instr)))

(define (arg1 instr)
  (unless (label? instr)
	(second instr)))


(define arg2 third)

(define set-arg1! set-second!)



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


