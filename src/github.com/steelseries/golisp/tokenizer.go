// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file impliments the parser

package golisp

import (
    "go/scanner"
    "go/token"
)

type Tokenizer struct {
    LookaheadToken token.Token
    LookaheadLit   string
    S              *scanner.Scanner
}

func NewTokenizer(src string) *Tokenizer {
    fset := token.NewFileSet()
    file := fset.AddFile("", fset.Base(), len(src))
    tok := &Tokenizer{S: &scanner.Scanner{}}
    tok.S.Init(file, []byte(src), nil, scanner.ScanComments)
    tok.ConsumeToken()
    return tok
}

func (self *Tokenizer) NextToken() (tok token.Token, lit string) {
    return self.LookaheadToken, self.LookaheadLit
}

func (self *Tokenizer) ConsumeToken() {
    _, self.LookaheadToken, self.LookaheadLit = self.S.Scan()
    for self.LookaheadToken == token.SEMICOLON {
        _, self.LookaheadToken, self.LookaheadLit = self.S.Scan()
    }
}
