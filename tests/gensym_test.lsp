;;; -*- mode: Scheme -*-

(describe gensym-with-default
          (assert-eq (gensym)
                     'GENSYM-1)
          (assert-eq (gensym)
                     'GENSYM-2)
          (assert-eq (gensym)
                     'GENSYM-3)
          (assert-eq (gensym)
                     'GENSYM-4))

(describe gensym-with-prefix
          (assert-eq (gensym 'hi)
                     'hi-1)
          (assert-eq (gensym "hi")
                     'hi-2)
          (assert-eq (gensym 'ho)
                     'ho-1)
          (assert-eq (gensym 'ho)
                     'ho-2)
          (assert-eq (gensym 'hi)
                     'hi-3))
