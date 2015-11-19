;;; -*- mode: Scheme -*-

;;; Copyright 2015 SteelSeries ApS. All rights reserved.
;;; Use of this source code is governed by a BSD-style
;;; license that can be found in the LICENSE file.

;;; Strings support llibrary
;;; Adds the rich set of standard Scheme string functions.  Only the
;;; most generic string comparison is builtin

(define (compare-false-proc)
    #f)

(define (compare-true-proc)
    #t)


(define (string=? string1 string2)
  (string-compare string1
                  string2
                  compare-false-proc
                  compare-true-proc
                  compare-false-proc))

(define (substring=? string1 start1 end1 string2 start2 end2)
  (string-compare (substring string1 start1 end1)
                  (substring string2 start2 end2)
                  compare-false-proc
                  compare-true-proc
                  compare-false-proc))

(define (string-ci=? string1 string2)
  (string-compare-ci string1
                     string2
                     compare-false-proc
                     compare-true-proc
                     compare-false-proc))

(define (substring-ci=? string1 start1 end1 string2 start2 end2)
  (string-compare-ci (substring string1 start1 end1)
                     (substring string2 start2 end2)
                     compare-false-proc
                     compare-true-proc
                     compare-false-proc))


(define (string<? string1 string2)
  (string-compare string1
                  string2
                  compare-true-proc
                  compare-false-proc
                  compare-false-proc))

(define (substring<? string1 start1 end1 string2 start2 end2)
  (string-compare (substring string1 start1 end1)
                  (substring string2 start2 end2)
                  compare-true-proc
                  compare-false-proc
                  compare-false-proc))

(define (string-ci<? string1 string2)
  (string-compare-ci string1
                     string2
                     compare-true-proc
                     compare-false-proc
                     compare-false-proc))

(define (substring-ci<? string1 start1 end1 string2 start2 end2)
  (string-compare-ci (substring string1 start1 end1)
                     (substring string2 start2 end2)
                     compare-true-proc
                     compare-false-proc
                     compare-false-proc))


(define (string<=? string1 string2)
  (string-compare string1
                  string2
                  compare-true-proc
                  compare-true-proc
                  compare-false-proc))

(define (substring<=? string1 start1 end1 string2 start2 end2)
  (string-compare (substring string1 start1 end1)
                  (substring string2 start2 end2)
                  compare-true-proc
                  compare-true-proc
                  compare-false-proc))

(define (string-ci<=? string1 string2)
  (string-compare-ci string1
                     string2
                     compare-true-proc
                     compare-true-proc
                     compare-false-proc))

(define (substring-ci<=? string1 start1 end1 string2 start2 end2)
  (string-compare-ci (substring string1 start1 end1)
                     (substring string2 start2 end2)
                     compare-true-proc
                     compare-true-proc
                     compare-false-proc))


(define (string>? string1 string2)
  (string-compare string1
                  string2
                  compare-false-proc
                  compare-false-proc
                  compare-true-proc))

(define (substring>? string1 start1 end1 string2 start2 end2)
  (string-compare (substring string1 start1 end1)
                  (substring string2 start2 end2)
                  compare-false-proc
                  compare-false-proc
                  compare-true-proc))

(define (string-ci>? string1 string2)
  (string-compare-ci string1
                     string2
                     compare-true-proc
                     compare-false-proc
                     compare-false-proc))

(define (substring-ci>? string1 start1 end1 string2 start2 end2)
  (string-compare-ci (substring string1 start1 end1)
                     (substring string2 start2 end2)
                     compare-false-proc
                     compare-false-proc
                     compare-true-proc))


(define (string>=? string1 string2)
  (string-compare string1
                  string2
                  compare-false-proc
                  compare-true-proc
                  compare-true-proc))

(define (substring>=? string1 start1 end1 string2 start2 end2)
  (string-compare (substring string1 start1 end1)
                  (substring string2 start2 end2)
                  compare-false-proc
                  compare-true-proc
                  compare-true-proc))

(define (string-ci>=? string1 string2)
  (string-compare-ci string1
                     string2
                     compare-false-proc
                     compare-true-proc
                     compare-true-proc))

(define (substring-ci>=? string1 start1 end1 string2 start2 end2)
  (string-compare-ci (substring string1 start1 end1)
                     (substring string2 start2 end2)
                     compare-false-proc
                     compare-true-proc
                     compare-true-proc))




