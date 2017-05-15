;;; -*- mode: Scheme -*-

(context "eqv?"

         ()

         (it "conforms to the MIT Scheme reference"
             (assert-true (eqv? 'a 'a))
             (assert-false (eqv? 'a 'b))
             (assert-true (eqv? 2 2))
             (assert-true (eqv? '() '()))
             (assert-true (eqv? 100000000 100000000))
             (assert-false (eqv? (cons 1 2) (cons 1 2)))
             (assert-false (eqv? (lambda () 1) (lambda () 1)))
             (assert-false (eqv? #f 'nil))
             (let ((p (lambda (x) x)))
               (assert-true (eqv? p p)))
             (assert-false (eqv? nan nan)))

         (it "has defined implementation specific behavior"
             (assert-false (eqv? "" ""))
             (assert-false (eqv? "a" "a"))
             (assert-false (eqv? '#() '#()))
             (assert-false (eqv? '(a) '(a)))))

(context "eq?"

         ()

         (it "has behavior"
             (assert-true (eq? 'a 'a))
             (assert-false (eq? '(a) '(a)))
             (assert-false (eq? (list 'a) (list 'a)))
             (assert-true (eq? "a" "a"))
             (assert-true (eq? "" ""))
             (assert-true (eq? '() '()))
             (assert-true (eq? 2 2))
             (assert-true (eq? car car))
             (assert-true (let ((n (+ 2 3)))
                            (eq? n n)))
             (assert-true (let ((x '(a)))
                             (eq? x x)))
             (assert-true (let ((x '#()))
                            (eq? x x)))
             (assert-true (let ((p (lambda (x) x)))
                            (eq? p p)))
             (assert-false (eq? nan nan))))

(context "equal?"

         ()

         (assert-true (equal? 'a 'a))
         (assert-true (equal? '(a) '(a)))
         (assert-true (equal? '#(a) '#(a)))
         (assert-true (equal? '(a (b) c)
                              '(a (b) c)))
         (assert-true (equal? "abc" "abc"))
         (assert-true (equal? 2 2))
         (assert-true (equal? (make-vector 5 'a)
                               (make-vector 5 'a)))
         (assert-false (equal? (lambda (x) x)
                               (lambda (y) y)))
         (assert-false (equal? nan nan)))
