(describe quasiquoted-literal
          (== `a 'a)
          (== `1 1))

(describe quasiquoted-list
          (== `(a b c) '(a b c))
          (== `(1 (2) 3) '(1 (2) 3)))

(describe unquote
          (== `(a ,(+ 1 2) b) '(a 3 b)))

(describe unquote-splicing
          (== `(a ,@(list 1 2 3) b) '(a 1 2 3 b)))

(describe combined-and-eval
          (let ((x 1)
                (y '(2 3)))
            (== (eval `(+ ,x ,@y)) 6)))

(describe defmacro
          (defmacro (add x y)
            `(+ ,x ,@y))

          (== (add 1 '(2 3)) 6))
