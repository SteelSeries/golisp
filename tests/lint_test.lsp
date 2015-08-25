;;; -*- mode: Scheme -*-

;;; Copyright 2015 SteelSeries ApS. All rights reserved.
;;; Use of this source code is governed by a BSD-style
;;; license that can be found in the LICENSE file.

(load "lisp/linting.lsp")

(context "Checking for mutators"

         ((define no-set-code '((car '(a b c))))
          (define set-code '((set! x 3)))
          (define setcar-code '((set-car! x 3)))
          (define setcdr-code '((set-cdr! x 3)))
          (define all-code (append set-code setcar-code setcdr-code)))

         (it "find nothing with no sets"
             (assert-nil (lint:analyze-set no-set-code)))
         
         (it "finds set!"
             (assert-eq (lint:analyze-set set-code) '("Mutator found: (set! x 3)")))

         (it "finds set!"
             (assert-eq (lint:analyze-set setcar-code) '("Mutator found: (set-car! x 3)")))
         
         (it "finds set!"
             (assert-eq (lint:analyze-set setcdr-code) '("Mutator found: (set-cdr! x 3)")))

         (it "finds all"
             (assert-memq (lint:analyze-set all-code) "Mutator found: (set! x 3)")
             (assert-memq (lint:analyze-set all-code) "Mutator found: (set-car! x 3)")
             (assert-memq (lint:analyze-set all-code) "Mutator found: (set-cdr! x 3)")))

(context "Checking for dependant let bindings"

         ((define bugged-let-code '((let ((a 1) (b (+ a 1))) (+ a b))))
          (define ok-let-code '((let ((a 1) (b (+ c 1))) (+ a b)))))

         (it "finds a backward dependancy"
             (assert-eq (lint:analyze-let bugged-let-code) '("Back reference in a LET: b")))
         
         (it "finds no a backward dependancy"
             (assert-nil (lint:analyze-let ok-let-code))))

(context "Checking for dependant do bindings"

         ((define bugged-do-code '((do ((a 1 2) (b (+ a 1) 3)) (#t) (+ a b))))
          (define ok-do-code '((do ((a 1 2) (b 2 3)) (#t) (+ a b)))))

         (it "finds a backward dependancy"
             (assert-eq (lint:analyze-do bugged-do-code) '("Back reference in a DO: b")))
         
         (it "finds no a backward dependancy"
             (assert-nil (lint:analyze-do ok-do-code))))


(context "checking for single clause ifs"

         ((define just-true-clause '((if #t 1)))
          (define nil-true-clause '((if #t () 1)))
          (define nil-false-clause '((if #t 1 ())))
          (define nested-if-true-code '((if #t (if #t 1) 0)))
          (define nested-if-false-code '((if #t 0 (if #t 1)))))

         (it "finds one with only a true clause"
             (assert-eq (lint:analyze-if just-true-clause) '("Single clause IF: (if #t 1)")))

         (it "finds one with a nil true clause"
             (assert-eq (lint:analyze-if nil-true-clause) '("Nil true clause IF: (if #t () 1)")))

         (it "finds one with a nil false clause"
             (assert-eq (lint:analyze-if nil-false-clause) '("Nil false clause IF: (if #t 1 ())")))

         (it "finds problems in nested true clause"
             (assert-eq (lint:analyze-if nested-if-true-code) '("Single clause IF: (if #t 1)")))

         (it "finds problems in nested false clause"
             (assert-eq (lint:analyze-if nested-if-false-code) '("Single clause IF: (if #t 1)"))))
