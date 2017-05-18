;;; -*- mode: Scheme -*-

;;; Copyright 2015 SteelSeries ApS. All rights reserved.
;;; Use of this source code is governed by a BSD-style
;;; license that can be found in the LICENSE file.

;;; Strings support llibrary
;;; Adds the rich set of standard Scheme string functions.  Only the
;;; basic string functions are builtin


(define (make-string count . character-arg)   
  (let ((element-string (cond ((null? character-arg) #\space)
                              ((eq? (length character-arg) 1) (car character-arg))
                              (else (error "make-string expected a single character")))))
    (list->string (make-list count element-string))))


(define string str)


(define (string-copy s)
  (substring s 0 (string-length s)))


(define (string-null? s)
  (zero? (string-length s)))


(define (string-head string end)
  (substring string 0 end))


(define (string-tail string start)
  (substring string start (string-length string)))


(define (string-pad-left string k . maybe-char)
  (let ((length (string-length string)))
    (cond ((<= k length) (substring string (- length k) length))
          (else (let ((padding-size (- k length))
                      (padding-char (if (null? maybe-char) #\space (car maybe-char))))
                  (string-append (make-string padding-size padding-char) string))))))


(define (string-pad-right string k . maybe-char)
  (let ((length (string-length string)))
    (cond ((<= k length) (substring string 0 k))
          (else (let ((padding-size (- k length))
                      (padding-char (if (null? maybe-char) #\space (car maybe-char))))
                  (string-append string (make-string padding-size padding-char)))))))


;;; ================================================================================
;;; Extended comparison functions

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


(define (substring-capitalized? string start end)
  (string-capitalized? (substring string start end)))


(define (substring-upper-case? string start end)
  (string-upper-case? (substring string start end)))


(define (substring-lower-case? string start end)
  (string-lower-case? (substring string start end)))

