;;; -*- mode: Scheme -*-

;;; Copyright 2015 SteelSeries ApS. All rights reserved.
;;; Use of this source code is governed by a BSD-style
;;; license that can be found in the LICENSE file.

;;; Conditionals

(define-macro (if condition-expr then-expr . else-expr)
  `(cond (,condition-expr ,then-expr)
         (else ,(car else-expr))))

(define-macro (when condition-expr . body)
  `(cond (,condition-expr ,@body)
         (else nil)))

(define-macro (unless condition-expr . body)
  `(cond ((not ,condition-expr) ,@body)
        (else nil)))
