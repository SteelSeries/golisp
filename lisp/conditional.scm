;;; -*- mode: Scheme -*-

;;; Copyright 2015 SteelSeries ApS. All rights reserved.
;;; Use of this source code is governed by a BSD-style
;;; license that can be found in the LICENSE file.

;;; Conditionals are all defined in terms of if


(define-macro (when condition-expr . body)
  `(if ,condition-expr
     (begin ,@body)
     nil))


(define-macro (unless condition-expr . body)
  `(if ,condition-expr
     nil
     (begin ,@body)))


