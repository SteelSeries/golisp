;;; -*- mode: Scheme -*-

;;; Copyright 2015 SteelSeries ApS. All rights reserved.
;;; Use of this source code is governed by a BSD-style
;;; license that can be found in the LICENSE file.

;;; Charset support library
;;; Supports the use of charset creation and constants for use with
;;; the string trim functions.  Those functions use a string rather
;;; than the standard charset object. The functions & constants here
;;; wrap that with the standard char-set verbage.

(load "lisp/strings.scm")

(define char-set string)

(define char-set:upper-case "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(define char-set:lower-case "abcdefghijklmnopqrstuvwxyz")
(define char-set:alphabetic "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
(define char-set:numeric "0123456789")
(define char-set:alphanumeric "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
(define char-set:whitespace (string #\space #\tab #\page #\linefeed #\return))
(define char-set:not-whitespace "!\"#$%&’()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_‘abcdefghijklmnopqrstuvwxyz{|}~")
(define char-set:graphic  "!\"#$%&’()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_‘abcdefghijklmnopqrstuvwxyz{|}~ ")
;(define char-set:not-graphic)
(define char-set:standard "!\"#$%&’()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_‘abcdefghijklmnopqrstuvwxyz{|}~ \n")
