;;; -*- mode: Scheme -*-

(define (foo a)
  (lambda (x) (* a x)))

(context "map"

         ()
         
         (it "map with returned lambda"
             (assert-eq (map (foo 5) '(1 2 3))
                        '(5 10 15)))

         (it "map with explicit lambda"
             (assert-eq (map (lambda (x) (* x 5)) '(1 2 3))
                        '(5 10 15)))

         (it "map with prim"
             (assert-eq (map car '((1 2) (3 4) (5 6)))
                        '(1 3 5)))

         (it "map with muliple lists"
             (assert-eq (map + '(1 2 3) '(4 5 6))
                        '(5 7 9)))

         (it "map errors"
             (assert-error (map 5 '( 1 2 3)))
             (assert-error (map + 4))
             (assert-error (map + '(1 2) 4 '(5 6))))

         (it "for-each"
             (let ((count 0))
               (assert-eq (for-each (lambda (x) (set! count (+ count x))) '(1 2 3 4))
                          '())
               (assert-eq count
                          10)))

         (it "for-each errors"
             (assert-error (for-each 5 '( 1 2 3))) ;1st arg must be a function
             (assert-error (for-each + 4)) ;remainign args must be lists
             (assert-error (for-each + '(1 2) 4 '(5 6)))) ;remaining args much be lists
)
