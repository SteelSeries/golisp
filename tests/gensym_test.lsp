(describe gensym-with-default
          (== (gensym) 'GENSYM1)
          (== (gensym) 'GENSYM2)
          (== (gensym) 'GENSYM3)
          (== (gensym) 'GENSYM4))

(describe gensym-with-prefix
          (== (gensym 'hi) 'hi1)
          (== (gensym "hi") 'hi2)
          (== (gensym 'ho) 'ho1)
          (== (gensym 'ho) 'ho2)
          (== (gensym 'hi) 'hi3))
