;;; -*- mode: Scheme -*-

(context "gensym"

         ()
         
         (it gensym-with-default
             (let ((first-sym gensym))
               (assert-neq (gensym)
                          first-sym)
               (assert-neq (gensym)
                          first-sym)
               (assert-neq (gensym)
                          first-sym)))

         (it gensym-with-prefix
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
                          hi-sym))))
