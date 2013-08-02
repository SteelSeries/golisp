(describe simple-let
          (== (let ())
              nil)
          (== (let ()
                42)
              42))

(describe let-with-multiple-expr-body
          (== (let ()
                1
                2)
              2))

(describe let-bindings
          (== (let ((x 1)
                    (y 2))
                (+ x y))
              3)
          (== (let ((x 1)
                    (y (+ x 1)))
                y)
              2))


(describe let-binding-scope
          (== (begin (let ((zz 2)) zz)
                     zz)
              nil))
