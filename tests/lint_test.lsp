;;; -*- mode: Scheme -*-

;;; Copyright 2015 SteelSeries ApS. All rights reserved.
;;; Use of this source code is governed by a BSD-style
;;; license that can be found in the LICENSE file.

(load "lisp/linting.lsp")

(context "Checking for mutators"

         ((define set-code '((set! x 3)))
          (define setcar-code '((set-car! x 3)))
          (define setcdr-code '((set-cdr! x 3)))
          (define all-code (append set-code setcar-code setcdr-code)))

         (it "finds set!"
             (assert-eq (lint:analyze-for-set set-code) '("Mutator found: (set! x 3)")))

         (it "finds set!"
             (assert-eq (lint:analyze-for-set setcar-code) '("Mutator found: (set-car! x 3)")))
         
         (it "finds set!"
             (assert-eq (lint:analyze-for-set setcdr-code) '("Mutator found: (set-cdr! x 3)")))

         (it "finds all"
             (assert-memq (lint:analyze-for-set all-code) "Mutator found: (set! x 3)")
             (assert-memq (lint:analyze-for-set all-code) "Mutator found: (set-car! x 3)")
             (assert-memq (lint:analyze-for-set all-code) "Mutator found: (set-cdr! x 3)")))
