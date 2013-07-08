// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file impliments the parser

package golisp

import (
    "unicode"
)

const (
    ILLEGAL = iota
    SYMBOL
    NUMBER
    HEXNUMBER
    STRING
    QUOTE
    LPAREN
    RPAREN
    PERIOD
    TRUE
    FALSE
    COMMENT
    EOF
)

type MyTokenizer struct {
    LookaheadToken int
    LookaheadLit   string
    Source         string
    Position       int
}

func NewMyTokenizer(src string) *MyTokenizer {
    t := &MyTokenizer{Source: src}
    t.ConsumeToken()
    return t
}

func (self *MyTokenizer) NextToken() (token int, lit string) {
    return self.LookaheadToken, self.LookaheadLit
}

func (self *MyTokenizer) isSymbolCharacter(ch rune) bool {
    return unicode.IsLetter(ch) || unicode.IsNumber(ch) || ch == '-' || ch == '?' || ch == '!' || ch == '_'
}

func (self *MyTokenizer) readSymbol() (token int, lit string) {
    start := self.Position
    for !self.isEof() && self.isSymbolCharacter(rune(self.Source[self.Position])) {
        self.Position++
    }
    return SYMBOL, self.Source[start:self.Position]
}

func isHex(ch rune) bool {
    switch ch {
    case 'a', 'b', 'c', 'd', 'e', 'f', 'A', 'B', 'C', 'D', 'E', 'F':
        return true
    default:
        return false
    }
}

func (self *MyTokenizer) readNumber() (token int, lit string) {
    start := self.Position
    hex := false
    for !self.isEof() {
        ch := rune(self.Source[self.Position])
        if (start == self.Position) && unicode.IsNumber(ch) {
            self.Position++
        } else if unicode.IsNumber(ch) {
            self.Position++
        } else if (start == self.Position-1) && ch == 'x' {
            hex = true
            self.Position++
        } else if hex && isHex(ch) {
            self.Position++
        } else {
            break
        }
    }

    if hex {
        return HEXNUMBER, self.Source[start:self.Position]
    } else {
        return NUMBER, self.Source[start:self.Position]
    }
}

func (self *MyTokenizer) readString() (token int, lit string) {
    buffer := make([]rune, 0, 10)
    self.Position++
    for !self.isEof() && rune(self.Source[self.Position]) != '"' {
        if rune(self.Source[self.Position]) == '\\' {
            self.Position++
        }
        buffer = append(buffer, rune(self.Source[self.Position]))
        self.Position++
    }
    if self.isEof() {
        return EOF, ""
    }
    self.Position++
    return STRING, string(buffer)
}

func (self *MyTokenizer) isEof() bool {
    return self.Position >= len(self.Source)
}

func (self *MyTokenizer) isAlmostEof() bool {
    return self.Position == len(self.Source)-1
}

func (self *MyTokenizer) readNextToken() (token int, lit string) {
    if self.isEof() {
        return EOF, ""
    }
    for unicode.IsSpace(rune(self.Source[self.Position])) {
        self.Position++
        if self.isEof() {
            return EOF, ""
        }
    }
    currentChar := rune(self.Source[self.Position])
    var nextChar rune
    if !self.isAlmostEof() {
        nextChar = rune(self.Source[self.Position+1])
    }
    if unicode.IsLetter(currentChar) || currentChar == '_' {
        return self.readSymbol()
    } else if unicode.IsNumber(currentChar) {
        return self.readNumber()
    } else if currentChar == '"' {
        return self.readString()
    } else if currentChar == '\'' {
        self.Position++
        return QUOTE, "'"
    } else if currentChar == '(' {
        self.Position++
        return LPAREN, "("
    } else if currentChar == ')' {
        self.Position++
        return RPAREN, ")"
    } else if currentChar == '.' {
        self.Position++
        return PERIOD, "."
    } else if currentChar == '+' {
        self.Position++
        return SYMBOL, "+"
    } else if currentChar == '-' {
        self.Position++
        return SYMBOL, "-"
    } else if currentChar == '*' {
        self.Position++
        return SYMBOL, "*"
    } else if currentChar == '/' {
        self.Position++
        return SYMBOL, "/"
    } else if currentChar == '%' {
        self.Position++
        return SYMBOL, "%"
    } else if currentChar == '<' && nextChar == '=' {
        self.Position += 2
        return SYMBOL, "<="
    } else if currentChar == '<' {
        self.Position++
        return SYMBOL, "<"
    } else if currentChar == '>' && nextChar == '=' {
        self.Position += 2
        return SYMBOL, ">="
    } else if currentChar == '>' {
        self.Position++
        return SYMBOL, ">"
    } else if currentChar == '=' && nextChar == '=' {
        self.Position += 2
        return SYMBOL, "=="
    } else if currentChar == '!' && nextChar == '=' {
        self.Position += 2
        return SYMBOL, "!="
    } else if currentChar == '!' {
        self.Position++
        return SYMBOL, "!"
    } else if currentChar == '#' {
        self.Position += 2
        if nextChar == 't' {
            return TRUE, "#t"
        } else {
            return FALSE, "#f"
        }
    } else if currentChar == ';' {
        start := self.Position
        for {
            if self.isEof() {
                return COMMENT, self.Source[start:]
            } else if self.Source[self.Position] == '\n' {
                return COMMENT, self.Source[start:self.Position]
            }
            self.Position++
        }
    } else {
        return ILLEGAL, ""
    }
}

func (self *MyTokenizer) ConsumeToken() {
    self.LookaheadToken, self.LookaheadLit = self.readNextToken()
}
