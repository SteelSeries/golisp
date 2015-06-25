;;; -*- mode: Scheme -*-

(describe quasiquoted-literal
          (assert-eq `a
                     'a)
          (assert-eq `1
                     1))

(describe quasiquoted-list
          (assert-eq `(a b c)
                     '(a b c))
          (assert-eq `(1 (2) 3)
                     '(1 (2) 3)))

(describe unquote
          (assert-eq `(a ,(+ 1 2) b)
                     '(a 3 b)))

(describe unquote-splicing
          (assert-eq `(a ,@(list 1 2 3) b)
                     '(a 1 2 3 b)))

(describe nested-unquote-splicing
          (assert-eq `(a ,@(list 1 2 3) `(list ,@(list a b c)))
                     '(a 1 2 3 `(list ,@(list a b c)))))

(describe combined-and-eval
          (let* ((x 1)
                 (y '(2 3)))
            (assert-eq (eval `(+ ,x ,@y))
                       6)))

(describe defmacro
          (defmacro (add x y)
            `(+ ,x ,@y))

          (assert-eq (add 1 '(2 3))
                     6))

(describe expand
          (defmacro (add x y)
            `(+ ,x ,@y))
          (assert-eq (expand add 1 '(2 3))
                     '(+ 1 2 3)))

(describe nested
          (assert-eq  `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f) 
                      '(a `(b ,(+ 1 2) ,(foo 4 d) e) f)))
