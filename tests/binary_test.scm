;;; -*- mode: Scheme -*-

(context "binary logic"

         ()

         
         (it "can and"
             (assert-eq (binary-and #x0a #x05)
                        #x00)
             (assert-eq (binary-and #x0a #x18)
                        #x08)
             (assert-eq (binary-and #xaa #xf0)
                        #xa0)

             (assert-error (binary-and 'a 2))
             (assert-error (binary-and '(a b) 2))
             (assert-error (binary-and 2 'a))
             (assert-error (binary-and 2 '(a b))))

         (it "can or"
             (assert-eq (binary-or #x0a #x05)
                        #x0f)
             (assert-eq (binary-or #x0a #x18)
                        #x1a)
             (assert-eq (binary-or #xaa #xf0)
                        #xfa)

             (assert-error (binary-or 'a 2))
             (assert-error (binary-or '(a b) 2))
             (assert-error (binary-or 2 'a))
             (assert-error (binary-or 2 '(a b))))

         (it "can negate"
             (assert-eq (binary-not #x0000000a)
                        #xfffffff5)
             (assert-eq (binary-not #x0a0a0a0a)
                        #xf5f5f5f5)
             
             (assert-error (binary-not 'a))
             (assert-error (binary-not '(a b))))

         (it "can left shift"
             (assert-eq (left-shift #x05 1)
                        #x0a)
             (assert-eq (left-shift #x05 2)
                        #x14)
             (assert-eq (left-shift #x05 3)
                        #x28)
             (assert-eq (left-shift #x05 4)
                        #x50)

             (assert-error (left-shift 'a 2))
             (assert-error (left-shift '(a b) 2))
             (assert-error (left-shift 2 'a))
             (assert-error (left-shift 2 '(a b))))

         (it "can right shift"
             (assert-eq (right-shift #x50 1)
                        #x28)
             (assert-eq (right-shift #x50 2)
                        #x14)
             (assert-eq (right-shift #x50 3)
                        #x0a)
             (assert-eq (right-shift #x50 4)
                        #x05)
             (assert-eq (right-shift #x0f 2)
                        #x03)

             (assert-error (right-shift 'a 2))
             (assert-error (right-shift '(a b) 2))
             (assert-error (right-shift 2 'a))
             (assert-error (right-shift 2 '(a b))))
)
