;;; -*- mode: Scheme -*-

;;; Copyright 2015 SteelSeries ApS. All rights reserved.
;;; Use of this source code is governed by a BSD-style
;;; license that can be found in the LICENSE file.

(define time first)
(define mode second)
(define type third)
(define name fourth)

(define node-time first)
(define node-type second)
(define node-name third)
(define node-children fourth)

(define call-tree nil)

(define (read-function-call start-data f)
  (let* ((start-time (time start-data))
         (f-type (type start-data))
         (f-name (name start-data))
         (children-end (let loop
                           ((d (read f))
                            (ch '()))
                         (cond ((and (eq? (mode d) 'exit)
                                     (eq? (name d) f-name))
                                (list (time d) ch))
                               ((eq? (mode d) 'enter)
                                (let ((children (cons (read-function-call d f) ch))) 
                                  (loop (read f) children))))))
         (end-time (car children-end))
         (children (cadr children-end)))
    (list (- end-time start-time) f-type f-name children)))


(define (load-profile filename)
  (let* ((f (open-input-file filename)))
    (set! call-tree (read-function-call (read f) f))
    (close-port f)))


(define (add-to-function fname time data)
  (let ((existing (assoc fname data)))
    (if (null? existing)
        (acons fname (list 1 time) data)
        (acons fname (list (+ (cadr existing) 1) (+ time (caddr existing))) (dissoc fname data)))))


;;; type can be prim, func, or all
(define (by-function f-type)
  (define (compute-by-function tree f-type data) 
    (let ((new-data (if (or (eq? f-type 'all)
                            (eq? f-type (node-type tree)))
                        (add-to-function (node-name tree) (node-time tree) data)
                        data)))
      (let loop
          ((d new-data)
           (nodes (node-children tree)))
        (if (null? nodes)
            d
            (loop (compute-by-function (car nodes) f-type d) (cdr nodes))))))

  (let ((call-data (compute-by-function call-tree f-type '())))
    (format #t "~12A | ~5@A | ~10@A~%" "Function" "count" "time (nS)")
    (format #t "-------------+-------+-----------~%")
    (for-each (lambda (d)
                (format #t "~12A | ~5@A | ~10@A~%" (car d) (cadr d) (caddr d)))
              call-data)))


(define (print-tree)
  (define (inner-print-tree node level)
    (format #t "~VA~A (~A)~%" (* 2 level) "" (node-name node) (node-time node))
    (for-each (lambda (n)
                (inner-print-tree n (+ level 1)))
              (node-children node)))

  (inner-print-tree call-tree 0))
