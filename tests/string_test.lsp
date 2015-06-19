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

(describe string-trim
          (== (string-trim "  hello ") "hello")
          (== (string-trim "++ yo --" "+-") " yo ")
          (== (string-trim "++ yo --" "+- ") "yo"))

(describe string-trim-left
          (== (string-trim-left "  hello ") "hello ")
          (== (string-trim-left "++ yo --" "+-") " yo --")
          (== (string-trim-left "++ yo --" "+- ") "yo --"))

(describe string-trim-right
          (== (string-trim-right "  hello ") "  hello")
          (== (string-trim-right "++ yo --" "+-") "++ yo ")
          (== (string-trim-right "++ yo --" "+- ") "++ yo"))

(describe string-upcase
          (== (string-upcase "hello") "HELLO")
          (== (string-upcase "HeLlo") "HELLO")
          (== (string-upcase "HELLO") "HELLO"))

(describe string-downcase
          (== (string-downcase "hello") "hello")
          (== (string-downcase "HeLlo") "hello")
          (== (string-downcase "HELLO") "hello"))

(describe string-capitalize
          (== (string-capitalize "hello") "Hello")
          (== (string-capitalize "HeLlo") "Hello")
          (== (string-capitalize "HELLO") "Hello"))

(describe string-upcase!
          (let ((s "hello"))
            (== (string-upcase! s) "HELLO")
            (== s "HELLO")))

(describe downcase!
          (let ((s "HELLO"))
            (== (string-downcase! s) "hello")
            (== s "hello")))

(describe string-capitalize!
          (let ((s "hello"))
            (== (string-capitalize! s) "Hello")
            (== s "Hello")))


(describe string-length
          (== (string-length "") 0)
          (== (string-length "1") 1)
          (== (string-length "12345") 5))


(describe string-null?
          (== (string-null? "") #t)
          (== (string-null? "hello") #f))

(describe substring
          (== (substring "hello" 0 0) "")
          (== (substring "arduous" 2 5) "duo"))


(describe substring?
          (== (substring? "rat" "pirate") #t)
          (== (substring? "rat" "outrage") #f)
          (== (substring? "" "hjdfgds") #t))

(describe string-prefix?
          (== (string-prefix? "abc" "abcdef") #t)
          (== (string-prefix? "abz" "abcdef") #f)
          (== (string-prefix? "" "akjsdfh") #t))

(describe string-suffix?
          (== (string-suffix? "def" "abcdef") #t)
          (== (string-suffix? "dez" "abcdef") #f)
          (== (string-suffix? "" "akjsdfh") #t))
