(describe str
          (== (str '()) "()")
          (== (str 0) "0")
          (== (str 1.4) "1.4")
          (== (str "1.0") "1.0")
          (== (str "hi") "hi")
          (== (str 'a) "a")
          (== (str '(1 2)) "(1 2)")
          (== (str '(1 . 2)) "(1 . 2)")
          (== (str (alist '((a . 1)))) "((a . 1))")
          (== (str "abc" 1 "-" 34.2 '(a b c)) "abc1-34.2(a b c)"))

(describe string->number
          (== (string->number "10") 10)
          (== (string->number "10" 2) 2)
          (== (string->number "10" 8) 8)
          (== (string->number "10" 10) 10)
          (== (string->number "10" 16) 16)
          (== (string->number "10" 20) 0))

(describe number->string
          (== (number->string 10) "10")
          (== (number->string 2 2) "10")
          (== (number->string 8 8) "10")
          (== (number->string 10 10) "10")
          (== (number->string 16 16) "10")
          (== (number->string 20 20) "Unsupported base: 20"))

(describe split
          (== (split "1-2" "-") '( "1" "2"))
          (== (split "one,two" ",") '("one" "two")))

(describe trim
          (== (trim "  hello ") "hello")
          (== (trim "++ yo --" "+-") " yo ")
          (== (trim "++ yo --" "+- ") "yo"))

(describe upcase
          (== (string-upcase "hello") "HELLO")
          (== (string-upcase "HeLlo") "HELLO")
          (== (string-upcase "HELLO") "HELLO"))

(describe downcase
          (== (string-downcase "hello") "hello")
          (== (string-downcase "HeLlo") "hello")
          (== (string-downcase "HELLO") "hello"))
