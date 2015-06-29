;;; -*- mode: Scheme -*-

;;; Copyright 2015 SteelSeries ApS. All rights reserved.
;;; Use of this source code is governed by a BSD-style
;;; license that can be found in the LICENSE file.


;;; --------------------------------------------------------------------------------
;;; Data Abstraction

(define record-time first)
(define record-guid second)
(define record-mode third)
(define record-type fourth)
(define record-name fifth)

(define make-node list)
(define node-time first)
(define node-type second)
(define node-name third)
(define node-children fourth)


;;; --------------------------------------------------------------------------------
;;; Global data

(define call-trees nil)


;;; --------------------------------------------------------------------------------
;;; Load profile data from a file and construct the call tree(s)

(define (load-profile filename)
  (define (read-function-call start-data f)
    (let* ((start-time (record-time start-data))
           (f-guid (record-guid start-data))
           (f-type (record-type start-data))
           (f-name (record-name start-data))
           (children-end (let loop
                             ((d (read f))
                              (ch '()))
                           (cond ((eof-object? d)
                                  d)
                                 ((and (eq? (record-mode d) 'exit)
                                       (eq? (record-guid d) f-guid))
                                  (list (record-time d) ch))
                                 ((eq? (record-mode d) 'enter)
                                  (let ((children (cons (read-function-call d f) ch))) 
                                    (loop (read f) children))))))
           (end-time (car children-end))
           (children (cadr children-end)))
      (if (eof-object? children-end)
          children-end
          (make-node (- end-time start-time) f-type f-name children))))

  (define (read-profile-data f)
    (let loop ((result '()))
      (let ((tree (read-function-call (read f) f)))
        (if (eof-object? tree)
            result
            (loop (cons tree result))))))

  (let ((f (open-input-file filename)))
    (set! call-trees (read-profile-data f))
    (close-port f)))


;;; --------------------------------------------------------------------------------
;;; produce a report summarizing data on each function called
;;; type can be prim, func, or all

(define (by-function f-type)

  (define (add-to-function fname time data)
    (let ((existing (assoc fname data)))
      (if (null? existing)
          (acons fname (list 1 time) data)
          (acons fname (list (+ (cadr existing) 1) (+ time (caddr existing))) (dissoc fname data)))))
  
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

  (let ((name-width 20)
        (count-width 5)
        (total-width 10)
        (avg-width 10)
        (call-data (let loop ((trees call-trees)
                              (data '()))
                     (if (nil? trees)
                         data
                         (loop (cdr trees) (compute-by-function (car trees) f-type data))))))
    (format #t "~VA | ~V@A | ~V@A | ~VA~%" name-width "Function" count-width "count" total-width "total time" avg-width "avg time")
    (format #t "~V~+~V~+~V~+~V~~%" (+ 1 name-width) (+ 2 count-width) (+ 2 total-width) (+ 1 avg-width))
    (for-each (lambda (d)
                (let ((name (car d))
                      (count (cadr d))
                      (time (caddr d)))
                  (format #t "~VA | ~V@A | ~V@A | ~VA~%" name-width name count-width count total-width time avg-width (/ time count))))
              call-data)
    (format #t "~V~+~V~+~V~+~V~~%" (+ 1 name-width) (+ 2 count-width) (+ 2 total-width) (+ 1 avg-width))))


;;; --------------------------------------------------------------------------------
;;; Print out the call tree

(define (print-tree)
  (define (inner-print-tree node level)
    (format #t "~VA~A (~A)~%" (* 2 level) "" (node-name node) (node-time node))
    (for-each (lambda (n)
                (inner-print-tree n (+ level 1)))
              (node-children node)))

  (for-each (lambda (tree)
              (inner-print-tree tree 0)
              (newline))
            call-trees))
