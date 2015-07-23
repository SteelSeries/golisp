;;; -*- mode: Scheme -*-

(defmacro (add x y)
  `(+ ,x ,@y))


(context "macro"

         ()
         
         (it quasiquoted-literal
             (assert-eq `a
                        'a)
             (assert-eq `1
                        1))

         (it quasiquoted-list
             (assert-eq `(a b c)
                        '(a b c))
             (assert-eq `(1 (2) 3)
                        '(1 (2) 3)))

         (it unquote
             (assert-eq `(a ,(+ 1 2) b)
                        '(a 3 b)))

         (it unquote-splicing
             (assert-eq `(a ,@(list 1 2 3) b)
                        '(a 1 2 3 b)))

         (it nested-unquote-splicing
             (assert-eq `(a ,@(list 1 2 3) `(list ,@(list a b c)))
                        '(a 1 2 3 `(list ,@(list a b c)))))

         (it combined-and-eval
             (let* ((x 1)
                    (y '(2 3)))
               (assert-eq (eval `(+ ,x ,@y))
                          6)))

         (it defmacro
             (assert-eq (add 1 (2 3))
                        6))

         (it expand
             (assert-eq (expand add 1 (2 3))
                        '(+ 1 2 3)))

         (it nested
             (assert-eq  `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f) 
                         '(a `(b ,(+ 1 2) ,(foo 4 d) e) f)))

         (it defmacro-errors
             (assert-error (defmacro "x" 1))
             (assert-error (defmacro ("x") 1)))
)
