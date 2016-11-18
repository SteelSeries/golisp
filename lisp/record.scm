;;; -*- mode: Scheme -*-

;;; Copyright 2016 Dave Astels. All rights reserved.
;;; Use of this source code is governed by a BSD-style
;;; license that can be found in the LICENSE file.

;;; Record support library
;;; Adds record-like support to frames

;;; defineing a "record" creates a proto with a new: fucntion slot that takes a value for each slot the record was defined with.
;;; It then creates a derivative frame and assigned the values to their respective slots (by position in the define-record call.

(define-macro (define-record record-name . slot-names)
  `(define ,record-name (make-frame
                         new: (lambda (first-val . rest-vals)
                                (let ((f (make-frame proto*: ,record-name)))
                                  (map (lambda (name val)
                                         (set-slot! f name val))
                                       ',slot-names
                                       (flatten (list first-val rest-vals)))
                                  f)))))
