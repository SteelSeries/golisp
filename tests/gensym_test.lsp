(describe gensym-with-default
          (== (gensym) 'GENSYM-1)
          (== (gensym) 'GENSYM-2)
          (== (gensym) 'GENSYM-3)
          (== (gensym) 'GENSYM-4))

(describe gensym-with-prefix
          (== (gensym 'hi) 'hi-1)
          (== (gensym "hi") 'hi-2)
          (== (gensym 'ho) 'ho-1)
          (== (gensym 'ho) 'ho-2)
          (== (gensym 'hi) 'hi-3))
