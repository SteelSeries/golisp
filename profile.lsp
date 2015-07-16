;;; -*- mode: Scheme -*-

;;; Copyright 2015 SteelSeries ApS. All rights reserved.
;;; Use of this source code is governed by a BSD-style
;;; license that can be found in the LICENSE file.



;;; --------------------------------------------------------------------------------
;;; Global data

(define call-trees nil)


;;; --------------------------------------------------------------------------------
;;; Load profile data from a file and construct the call tree(s)

(define (load-profile filename)

  (define (get-record f)
    (let ((d (read f)))
      (if (eof-object? d)
          d
          (eval d))))
  
  (define (read-function-call start-data f)
    (if (eof-object? start-data)
        start-data
        (let* ((start-time (time: start-data))
               (f-guid (guid: start-data))
               (f-type (type: start-data))
               (f-name (name: start-data))
               (children-end (let loop
                                 ((d (get-record f))
                                  (ch '()))
                               (cond ((eof-object? d)
                                      d)
                                     ((and (eq? (mode: d) 'exit)
                                           (eq? (guid: d) f-guid))
                                      (list (time: d) ch))
                                     ((eq? (mode: d) 'enter)
                                      (let* ((child (read-function-call d f))
                                             (children (cons child ch))) 
                                        (loop (get-record f) children))))))
               (end-time (if (eof-object? children-end) {time: 0} (car children-end)))
               (children (if (eof-object? children-end) {time: 0} (cadr children-end))))
          (if (eof-object? children-end)
              children-end
              {time: (- end-time start-time) type: f-type name: f-name children: children}))))
  
  (define (read-profile-data f)
    (let loop ((result '()))
      (let ((tree (read-function-call (get-record f) f)))
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
                            (eq? f-type (type: tree)))
                        (add-to-function (name: tree) (time: tree) data)
                       data)))
      (let loop
          ((d new-data)
           (nodes (children: tree)))
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
    (let ((has-children (not (null? (children: node))))
          (children-time (reduce + 0 (map (lambda (n) (time: n)) (children: node)))))
      (if has-children
          (format #t "~VA~A (total: ~A, overhead: ~A)~%" (* 2 level) "" (name: node) (time: node) (- (time: node) children-time))
          (format #t "~VA~A (total: ~A)~%" (* 2 level) "" (name: node) (time: node)))
      (for-each (lambda (n)
                  (inner-print-tree n (+ level 1)))
                (children: node))))

  (for-each (lambda (tree)
              (inner-print-tree tree 0)
              (newline))
            call-trees))
