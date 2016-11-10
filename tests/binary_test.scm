;;; -*- mode: Scheme -*-

(context "binary logic"

         ()

         
         (it "can and"
             (assert-eq (binary-and 0x0a 0x05)
                        0x00)
             (assert-eq (binary-and 0x0a 0x18)
                        0x08)
             (assert-eq (binary-and 0xaa 0xf0)
                        0xa0)

             (assert-error (binary-and 'a 2))
             (assert-error (binary-and '(a b) 2))
             (assert-error (binary-and 2 'a))
             (assert-error (binary-and 2 '(a b))))

         (it "can or"
             (assert-eq (binary-or 0x0a 0x05)
                        0x0f)
             (assert-eq (binary-or 0x0a 0x18)
                        0x1a)
             (assert-eq (binary-or 0xaa 0xf0)
                        0xfa)

             (assert-error (binary-or 'a 2))
             (assert-error (binary-or '(a b) 2))
             (assert-error (binary-or 2 'a))
             (assert-error (binary-or 2 '(a b))))

         (it "can negate"
             (assert-eq (binary-not 0x0000000a)
                        0xfffffff5)
             (assert-eq (binary-not 0x0a0a0a0a)
                        0xf5f5f5f5)
             
             (assert-error (binary-not 'a))
             (assert-error (binary-not '(a b))))

         (it "can left shift"
             (assert-eq (left-shift 0x05 1)
                        0x0a)
             (assert-eq (left-shift 0x05 2)
                        0x14)
             (assert-eq (left-shift 0x05 3)
                        0x28)
             (assert-eq (left-shift 0x05 4)
                        0x50)

             (assert-error (left-shift 'a 2))
             (assert-error (left-shift '(a b) 2))
             (assert-error (left-shift 2 'a))
             (assert-error (left-shift 2 '(a b))))

         (it "can right shift"
             (assert-eq (right-shift 0x50 1)
                        0x28)
             (assert-eq (right-shift 0x50 2)
                        0x14)
             (assert-eq (right-shift 0x50 3)
                        0x0a)
             (assert-eq (right-shift 0x50 4)
                        0x05)
             (assert-eq (right-shift 0x0f 2)
                        0x03)

             (assert-error (right-shift 'a 2))
             (assert-error (right-shift '(a b) 2))
             (assert-error (right-shift 2 'a))
             (assert-error (right-shift 2 '(a b))))
)
