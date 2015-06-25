;;; -*- mode: Scheme -*-

(describe str
          (assert-eq (str '())
                     "()")
          (assert-eq (str 0)
                     "0")
          (assert-eq (str 1.4)
                     "1.4")
          (assert-eq (str "1.0")
                     "1.0")
          (assert-eq (str "hi")
                     "hi")
          (assert-eq (str 'a)
                     "a")
          (assert-eq (str '(1 2))
                     "(1 2)")
          (assert-eq (str '(1 . 2))
                     "(1 . 2)")
          (assert-eq (str (alist '((a . 1))))
                     "((a . 1))")
          (assert-eq (str "abc" 1 "-" 34.2 '(a b c))
                     "abc1-34.2(a b c)"))

(describe string->number
          (assert-eq (string->number "10")
                     10)
          (assert-eq (string->number "10" 2)
                     2)
          (assert-eq (string->number "10" 8)
                     8)
          (assert-eq (string->number "10" 10)
                     10)
          (assert-eq (string->number "10" 16)
                     16)
          (assert-eq (string->number "10" 20)
                     0))

(describe number->string
          (assert-eq (number->string 10)
                     "10")
          (assert-eq (number->string 2 2)
                     "10")
          (assert-eq (number->string 8 8)
                     "10")
          (assert-eq (number->string 10 10)
                     "10")
          (assert-eq (number->string 16 16)
                     "10")
          (assert-eq (number->string 20 20)
                     "Unsupported base: 20"))

(describe string-split
          (assert-eq (string-split "1-2" "-")
                     '("1" "2"))
          (assert-eq (string-split "one,two" ",")
                     '("one" "two")))

(describe string-trim
          (assert-eq (string-trim "  hello ")
                     "hello")
          (assert-eq (string-trim "++ yo --" "+-")
                     " yo ")
          (assert-eq (string-trim "++ yo --" "+- ")
                     "yo"))

(describe string-trim-left
          (assert-eq (string-trim-left "  hello ")
                     "hello ")
          (assert-eq (string-trim-left "++ yo --" "+-")
                     " yo --")
          (assert-eq (string-trim-left "++ yo --" "+- ")
                     "yo --"))

(describe string-trim-right
          (assert-eq (string-trim-right "  hello ")
                     "  hello")
          (assert-eq (string-trim-right "++ yo --" "+-")
                     "++ yo ")
          (assert-eq (string-trim-right "++ yo --" "+- ")
                     "++ yo"))

(describe string-upcase
          (assert-eq (string-upcase "hello")
                     "HELLO")
          (assert-eq (string-upcase "HeLlo")
                     "HELLO")
          (assert-eq (string-upcase "HELLO")
                     "HELLO"))

(describe string-downcase
          (assert-eq (string-downcase "hello")
                     "hello")
          (assert-eq (string-downcase "HeLlo")
                     "hello")
          (assert-eq (string-downcase "HELLO")
                     "hello"))

(describe string-capitalize
          (assert-eq (string-capitalize "hello")
                     "Hello")
          (assert-eq (string-capitalize "HeLlo")
                     "Hello")
          (assert-eq (string-capitalize "HELLO")
                     "Hello"))

(describe string-upcase!
          (let ((s "hello"))
            (assert-eq (string-upcase! s)
                       "HELLO")
            (assert-eq s
                       "HELLO")))

(describe downcase!
          (let ((s "HELLO"))
            (assert-eq (string-downcase! s)
                       "hello")
            (assert-eq s
                       "hello")))

(describe string-capitalize!
          (let ((s "hello"))
            (assert-eq (string-capitalize! s)
                       "Hello")
            (assert-eq s
                       "Hello")))


(describe string-length
          (assert-eq (string-length "")
                     0)
          (assert-eq (string-length "1")
                     1)
          (assert-eq (string-length "12345")
                     5))


(describe string-null?
          (assert-true (string-null? ""))
          (assert-false (string-null? "hello")))

(describe substring
          (assert-eq (substring "hello" 0 0)
                     "")
          (assert-eq (substring "arduous" 2 5)
                     "duo"))


(describe substring?
          (assert-true (substring? "rat" "pirate"))
          (assert-false (substring? "rat" "outrage"))
          (assert-true (substring? "" "hjdfgds")))

(describe string-prefix?
          (assert-true (string-prefix? "abc" "abcdef"))
          (assert-false (string-prefix? "abz" "abcdef"))
          (assert-true (string-prefix? "" "akjsdfh")))

(describe string-suffix?
          (assert-true (string-suffix? "def" "abcdef"))
          (assert-false (string-suffix? "dez" "abcdef"))
          (assert-true (string-suffix? "" "akjsdfh")))
