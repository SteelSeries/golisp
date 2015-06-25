;;; -*- mode: Scheme -*-

(define (foo a)
  (lambda (x) (* a x)))

(describe map-with-constructed-lambda
          (assert-eq (map (foo 5) '(1 2 3))
                     '(5 10 15)))

(describe map-with-constructed-lambda
          (assert-eq (map (lambda (x) (* x 5)) '(1 2 3))
                     '(5 10 15)))

(describe map-with-prim
          (assert-eq (map car '((1 2) (3 4) (5 6)))
                     '(1 3 5)))

(describe map-with-muliple-lists
          (assert-eq (map + '(1 2 3) '(4 5 6))
                     '(5 7 9)))

(describe for-each
          (let ((count 0))
            (assert-eq (for-each (lambda (x) (set! count (+ count x))) '(1 2 3 4))
                       '())
            (assert-eq count
                       10)))
