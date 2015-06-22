;;; -*- mode: Scheme -*-

;;; Copyright 2015 SteelSeries ApS. All rights reserved.
;;; Use of this source code is governed by a BSD-style
;;; license that can be found in the LICENSE file.

(define time car)
(define mode cadr)
(define type caddr)
(define name cadddr)

(define node-time car)
(define node-type cadr)
(define node-name caddr)
(define node-children cadddr)


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
                              (loop (read f) (cons (read-function-call d f) ch))))))
           (children (cadr children-end))
           (end-time (car children-end)))
      (list (- end-time start-time) f-type f-name children)))

(define (load-profile-file filename)
    (let* ((f (open-input-file filename))
           (call-tree (read-function-call (read f) f)))
      (close-port f)
      call-tree))


(define (add-to-function fname time data)
  (let ((existing (assoc fname data)))
    (if (null? existing)
        (acons fname time data)
        (acons fname (+ time (cadr existing)) (dissoc fname data)))))


;;; type can be prim, func, or all
(define (report-by-function call-tree f-type data)
  (let ((new-data (if (or (eq? f-type 'all)
                          (eq? f-type (node-type call-tree)))
                      (add-to-function (node-name call-tree) (node-time call-tree) data)
                      data)))
    (let loop
        ((d (new-data))
         (nodes (node-children call-tree)))
      (if (null? nodes)
          d
          (loop (report-by-function (car nodes) f-type d) (cdr nodes))))))
