;;; -*- mode: Scheme -*-

(describe binary-and
          (assert-eq (binary-and 0x0a 0x05)
                     0x00)
          (assert-eq (binary-and 0x0a 0x18)
                     0x08)
          (assert-eq (binary-and 0xaa 0xf0)
                     0xa0))

(describe binary-or
          (assert-eq (binary-or 0x0a 0x05)
                     0x0f)
          (assert-eq (binary-or 0x0a 0x18)
                     0x1a)
          (assert-eq (binary-or 0xaa 0xf0)
                     0xfa))

(describe binary-not
          (assert-eq (binary-not 0x0000000a)
                     0xfffffff5)
          (assert-eq (binary-not 0x0a0a0a0a)
                     0xf5f5f5f5))

(describe left-shift
          (assert-eq (left-shift 0x05 1)
                     0x0a)
          (assert-eq (left-shift 0x05 2)
                     0x14)
          (assert-eq (left-shift 0x05 3)
                     0x28)
          (assert-eq (left-shift 0x05 4)
                     0x50))

(describe right-shift
          (assert-eq (right-shift 0x50 1)
                     0x28)
          (assert-eq (right-shift 0x50 2)
                     0x14)
          (assert-eq (right-shift 0x50 3)
                     0x0a)
          (assert-eq (right-shift 0x50 4)
                     0x05)
          (assert-eq (right-shift 0x0f 2)
                     0x03))
