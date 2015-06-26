;;; -*- mode: Scheme -*-

;;; Copyright 2015 SteelSeries ApS. All rights reserved.
;;; Use of this source code is governed by a BSD-style
;;; license that can be found in the LICENSE file.

(define time first)
(define guid second)
(define mode third)
(define type fourth)
(define name fifth)

(define node-time first)
(define node-type second)
(define node-name third)
(define node-children fourth)

(define call-trees nil)


(define (load-profile filename)
  (define (read-function-call start-data f)
    (let* ((start-time (time start-data))
           (f-guid (guid start-data))
           (f-type (type start-data))
           (f-name (name start-data))
           (children-end (let loop
                             ((d (read f))
                              (ch '()))
                           (cond ((eof-object? d)
                                  d)
                                 ((and (eq? (mode d) 'exit)
                                       (eq? (guid d) f-guid))
                                  (list (time d) ch))
                                 ((eq? (mode d) 'enter)
                                  (let ((children (cons (read-function-call d f) ch))) 
                                    (loop (read f) children))))))
           (end-time (car children-end))
           (children (cadr children-end)))
      (if (eof-object? children-end)
          children-end
          (list (- end-time start-time) f-type f-name children))))

  (define (read-profile-data f)
    (let loop ((result '()))
      (let ((tree (read-function-call (read f) f)))
        (if (eof-object? tree)
            result
            (loop (cons tree result))))))

  (let ((f (open-input-file filename)))
    (set! call-trees (read-profile-data f))
    (close-port f)))




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

  (let ((call-data (let loop ((trees call-trees)
                              (data '()))
                     (if (nil? trees)
                         data
                         (loop (cdr trees) (compute-by-function (car trees) f-type data))))))
    (format #t "~20A | ~5@A | ~10@A | ~10A~%" "Function" "count" "total time" "avg time")
    (format #t "---------------------+-------+------------+-----------~%")
    (for-each (lambda (d)
                (let ((name (car d))
                      (count (cadr d))
                      (time (caddr d)))
                  (format #t "~20A | ~5@A | ~10@A | ~10A~%" name count time (/ time count))))
              call-data)
    (format #t "---------------------+-------+------------+-----------~%")))


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
