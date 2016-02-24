;;; -*- mode: Scheme -*-

(load "lisp/strings.lsp")

(context "string"

         ((define lowercase-string "abcdefg")
          (define uppercase-string "ABCDEFG"))
         
         (it "str"
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
             (assert-eq (str '((a . 1)))
                        "((a . 1))")
             (assert-eq (str "abc" 1 "-" 34.2 '(a b c))
                        "abc1-34.2(a b c)"))

         (it "string->number"
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
             (assert-error (string->number "10" 20)))

         (it "number->string"
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
             (assert-error (number->string 20 20)))

         (it "string-split"
             (assert-eq (string-split "1-2" "-")
                        '("1" "2"))
             (assert-eq (string-split "one,two" ",")
                        '("one" "two"))
             (assert-error (string-split 3 ""))
             (assert-error (string-split "" 3)))

         (it "string-length"
             (assert-eq (string-length "")
                        0)
             (assert-eq (string-length "1")
                        1)
             (assert-eq (string-length "12345")
                        5)
             (assert-error (string-length 5)))


         (it "string-null?"
             (assert-true (string-null? ""))
             (assert-false (string-null? "hello"))
             (assert-error (string-null? 5)))

         (it "substring"
             (assert-eq (substring "hello" 0 0)
                        "")
             (assert-eq (substring "arduous" 2 5)
                        "duo")
             (assert-error (substring 5 1 2))
             (assert-error (substring "hello" "a" 5))
             (assert-error (substring "hello" 1 "5"))
             (assert-error (substring "hello" 10 2))
             (assert-error (substring "hello" 1 10)))


         (it "substring?"
             (assert-true (substring? "rat" "pirate"))
             (assert-false (substring? "rat" "outrage"))
             (assert-true (substring? "" "hjdfgds"))
             (assert-error (substring? 5 5))
             (assert-error (substring? "" 3))
             (assert-error (substring? 3 "")))

         (it "string-prefix?"
             (assert-true (string-prefix? "abc" "abcdef"))
             (assert-false (string-prefix? "abz" "abcdef"))
             (assert-true (string-prefix? "" "akjsdfh"))
             (assert-error (string-prefix? 5 5))
             (assert-error (string-prefix? "" 3))
             (assert-error (string-prefix? 3 "")))

         (it "string-suffix?"
             (assert-true (string-suffix? "def" "abcdef"))
             (assert-false (string-suffix? "dez" "abcdef"))
             (assert-true (string-suffix? "" "akjsdfh"))
             (assert-error (string-suffix? 5 5))
             (assert-error (string-suffix? "" 3))
             (assert-error (string-suffix? 3 "")))

         (it "can convert a substring to uppercase in place"
             (let  ((lowercase-string "abcdefg"))
               (assert-eq (substring-upcase! lowercase-string 3 5) "abcDEfg")
               (assert-eq lowercase-string "abcDEfg"))))


(context "Downcasing"

         ()

         (it "can convert to lowercase"
             (assert-eq (string-downcase "hello") "hello")
             (assert-eq (string-downcase "HeLlo") "hello")
             (assert-eq (string-downcase "HELLO") "hello")
             (assert-error (string-downcase 5)))

         (it "can convert to lowercase in place"
             (let ((s "HELLO"))
               (assert-eq (string-downcase! s) "hello")
               (assert-eq s "hello"))
             (assert-error (string-downcase! 5)))

         (it "can convert a substring to lowercase in place"
             (let  ((uppercase-string "ABCDEFG"))
               (assert-eq (substring-downcase! uppercase-string 3 5) "ABCdeFG")
               (assert-eq uppercase-string "ABCdeFG"))))

(context "Upcasing"

         ()

         (it "can convert to uppercase"
             (assert-eq (string-upcase "hello") "HELLO")
             (assert-eq (string-upcase "HeLlo") "HELLO")
             (assert-eq (string-upcase "HELLO") "HELLO")
             (assert-error (string-upcase 4)))

         (it "can convert to uppercase in place"
             (let ((s "hello"))
               (assert-eq (string-upcase! s) "HELLO")
               (assert-eq s "HELLO"))
             (assert-error (string-upcase! 5))))

(context "Capitalizing"

         ()
         
         (it "can capitalize"
             (assert-eq (string-capitalize "hello") "Hello")
             (assert-eq (string-capitalize "HeLlo") "Hello")
             (assert-eq (string-capitalize "HELLO") "Hello")
             (assert-error (string-capitalize 5)))

         (it "can capitalize in place"
             (let  ((lowercase-string "abcdefg")
                    (uppercase-string "ABCDEFG"))
               (assert-eq (string-capitalize! lowercase-string) "Abcdefg")
               (assert-eq lowercase-string "Abcdefg")
               (assert-eq (string-capitalize! uppercase-string) "Abcdefg")
               (assert-eq uppercase-string "Abcdefg")))

         (it "can capitalize a substring in place"
             (let  ((lowercase-string "abcdefg")
                    (uppercase-string "ABCDEFG"))
               (assert-eq (substring-capitalize! lowercase-string 3 5) "abcDefg")
               (assert-eq lowercase-string "abcDefg")
               (assert-eq (substring-capitalize! uppercase-string 3 5) "ABCDeFG")
               (assert-eq uppercase-string "ABCDeFG"))))

(context "string-trim"

         ()
         
         (it "defaults to trimming whitespace from both ends"
             (assert-eq (string-trim "  hello ") "hello"))
         
         (it "can trim everything but a set of characters from both ends"
             (assert-eq (string-trim "++ yo --" "abcdefghijklmnopqrstuvwxyz") "yo"))
         
         (it "raises an error when given a non string to trim"
             (assert-error (string-trim 3 "")))

         (it "raises an error when given a non string keep-set"
             (assert-error (string-trim "" 3))))

(context "string-trim-left"

         ()
         
         (it "defaults to trimming whitespace from the start"
             (assert-eq (string-trim-left "  hello ") "hello "))

         (it "can trim everything but a set of characters from the start"
             (assert-eq (string-trim-left "++ yo --" "abcdefghijklmnopqrstuvwxyz") "yo --"))

         (it "raises an error when given a non string to trim"
             (assert-error (string-trim-left 3 "")))

         (it "raises an error when given a non string keep-set"
             (assert-error (string-trim-left "" 3))))

(context "string-trim-right"

         ()
         
         (it "defaults to trimming whitespace from the end"
             (assert-eq (string-trim-right "  hello ") "  hello"))
         
         (it "can trim everything but a set of characters from the end"
             (assert-eq (string-trim-right "++ yo --" "abcdefghijklmnopqrstuvwxyz") "++ yo"))

         (it "raises an error when given a non string to trim"
             (assert-error (string-trim-right 3 "")))

         (it "raises an error when given a non string keep-set"
             (assert-error (string-trim-right "" 3))))


(context "String comparison"
         (it "can test string equality"
             (assert-true (string=? "a" "a"))
             (assert-false (string=? "a" "b"))
             (assert-false (string=? "a" "A"))
             (assert-true (string-ci=? "a" "A")))

         (it "can test string less than"
             (assert-true (string<? "a" "b"))
             (assert-false (string<? "b" "a"))
             (assert-false (string<? "a" "a"))
             (assert-true (string<? "A" "a"))
             (assert-false (string-ci<? "A" "a")))

         (it "can test string greater than"
             (assert-true (string>? "b" "a"))
             (assert-false (string>? "a" "b"))
             (assert-false (string>? "a" "a"))
             (assert-true (string>? "a" "A"))
             (assert-false (string-ci>? "a" "A")))

         (it "can test string less than equal"
             (assert-true (string<=? "a" "b"))
             (assert-false (string<=? "b" "a"))
             (assert-true (string<=? "a" "a"))
             (assert-true (string<=? "A" "a"))
             (assert-true (string-ci<=? "A" "a")))

         (it "can test string greater than equal"
             (assert-true (string>=? "b" "a"))
             (assert-false (string>=? "a" "b"))
             (assert-true (string>=? "a" "a"))
             (assert-true (string>=? "a" "A"))
             (assert-true (string-ci>=? "a" "A"))))

(context "Substring comparison"
         (it "can test string equality"
             (assert-true (substring=? "-a-" 1 2 "-a-" 1 2))
             (assert-false (substring=? "-a-" 1 2 "-b-" 1 2))
             (assert-false (substring=? "-a-" 1 2 "-A-" 1 2))
             (assert-true (substring-ci=? "-a-" 1 2 "-A-" 1 2)))

         (it "can test string less than"
             (assert-true (substring<? "-a-" 1 2 "-b-" 1 2))
             (assert-false (substring<? "-b-" 1 2 "-a-" 1 2))
             (assert-false (substring<? "-a-" 1 2 "-a-" 1 2))
             (assert-true (substring<? "-A-" 1 2 "-a-" 1 2))
             (assert-false (substring-ci<? "-A-" 1 2 "-a-" 1 2)))

         (it "can test string greater than"
             (assert-true (substring>? "-b-" 1 2 "-a-" 1 2))
             (assert-false (substring>? "-a-" 1 2 "-b-" 1 2))
             (assert-false (substring>? "-a-" 1 2 "-a-" 1 2))
             (assert-true (substring>? "-a-" 1 2 "-A-" 1 2))
             (assert-false (substring-ci>? "-a-" 1 2 "-A-" 1 2)))

         (it "can test string less than equal"
             (assert-true (substring<=? "-a-" 1 2 "-b-" 1 2))
             (assert-false (substring<=? "-b-" 1 2 "-a-" 1 2))
             (assert-true (substring<=? "-a-" 1 2 "-a-" 1 2))
             (assert-true (substring<=? "-A-" 1 2 "-a-" 1 2))
             (assert-true (substring-ci<=? "-A-" 1 2 "-a-" 1 2)))

         (it "can test string greater than equal"
             (assert-true (substring>=? "-b-" 1 2 "-a-" 1 2))
             (assert-false (substring>=? "-a-" 1 2 "-b-" 1 2))
             (assert-true (substring>=? "-a-" 1 2 "-a-" 1 2))
             (assert-true (substring>=? "-a-" 1 2 "-A-" 1 2))
             (assert-true (substring-ci>=? "-a-" 1 2 "-A-" 1 2))))

(context "Character access"

         ()

         (it "can extract characters"
             (assert-eq (string-ref "Hello" 1) #\e))

         (it "raises an error when given a non string"
             (assert-error (string-ref 'hi 1)))

         (it "raises an error when given a non integer"
             (assert-error (string-ref "Hello" 'a)))
         
         (it "raises an error when given an out of range index"
             (assert-error (string-ref "Hello" -1))
             (assert-error (string-ref "Hello" 10))))

(context "Character replacment"

         ()
         
         (it "can replace characters"
             (let ((a "Hello"))
               (assert-eq (string-set! a 3 #\O) "HelOo")
               (assert-eq a "HelOo")))

         (it "raises an error when given a non string"
             (assert-error (string-set! 'hi 1 #\e)))

         (it "raises an error when given a non integer"
             (assert-error (string-set! "Hello" 'a #\e)))

         (it "raises an error when given an out of range index"
             (assert-error (string-set! "Hello" -1 #\e))
             (assert-error (string-set! "Hello" 10 #\e)))

         (it "raises an error when given a non character"
             (assert-error (string-set! "Hello" 1 "a"))))
