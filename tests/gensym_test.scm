;;; -*- mode: Scheme -*-

(context "gensym"

         ()

         (it "gensym-with-default"
             (let ((first-sym (gensym)))
               (assert-neq (gensym)
                          first-sym)
               (assert-neq (gensym)
                          first-sym)
               (assert-neq (gensym)
                          first-sym)))

         (it "gensym-with-prefix"
             (let ((hi-sym (gensym 'hi))
                   (ho-sym (gensym 'ho)))
               (assert-neq (gensym 'hi)
                          hi-sym)
               (assert-neq (gensym "hi")
                          hi-sym)
               (assert-neq (gensym 'ho)
                          ho-sym)
               (assert-neq (gensym 'ho)
                          ho-sym)
               (assert-neq (gensym 'hi)
                          hi-sym)))

         (it "gensym-naked-with-default"
             (let ((first-sym (gensym-naked)))
               (assert-eq first-sym (eval first-sym))
               (assert-neq (gensym-naked)
                          first-sym)
               (assert-neq (gensym-naked)
                          first-sym)
               (assert-neq (gensym-naked)
                          first-sym)))

         (it "gensym-naked-with-prefix"
             (let ((hi-sym (gensym-naked 'hi))
                   (ho-sym (gensym-naked 'ho)))
               (assert-eq hi-sym (eval hi-sym))
               (assert-eq ho-sym (eval ho-sym))
               (assert-neq (gensym-naked 'hi)
                          hi-sym)
               (assert-neq (gensym-naked "hi")
                          hi-sym)
               (assert-neq (gensym-naked 'ho)
                          ho-sym)
               (assert-neq (gensym-naked 'ho)
                          ho-sym)
               (assert-neq (gensym-naked 'hi)
                          hi-sym))))
