// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file impliments the parser

package golisp

import (
    "errors"
    "fmt"
    "os"
)

func makeNumber(str string) (n *Data, err error) {
    var i uint32
    _, err = fmt.Sscanf(str, "%d", &i)
    if err != nil {
        return
    }
    n = NumberWithValue(i)
    return
}

func makeHexNumber(str string) (n *Data, err error) {
    var i uint32
    _, err = fmt.Sscanf(str, "%v", &i)
    if err != nil {
        return
    }
    n = NumberWithValue(i)
    return
}

func makeString(str string) (s *Data, err error) {
    s = StringWithValue(str[1 : len(str)-1])
    return
}

func makeSymbol(str string) (s *Data, err error) {
    s = SymbolWithName(str)
    return
}

func parseConsCell(s *MyTokenizer) (sexpr *Data, eof bool, err error) {
    tok, _ := s.NextToken()
    if tok == RPAREN {
        s.ConsumeToken()
        sexpr = nil
        return
    }

    var car *Data
    var cdr *Data
    cells := make([]*Data, 0, 10)
    for tok != RPAREN {
        if tok == PERIOD {
            s.ConsumeToken()
            cdr, eof, err = parseExpression(s)
            if eof || err != nil {
                return
            }
            tok, _ = s.NextToken()
            if tok != RPAREN {
                err = errors.New("Expected ')'")
                return
            }
            s.ConsumeToken()
            sexpr = ArrayToListWithTail(cells, cdr)
            return
        } else {
            car, eof, err = parseExpression(s)
            if eof {
                err = errors.New("Unexpected EOF (expected closing parenthesis)")
                return
            }
            if err != nil {
                return
            }
            cells = append(cells, car)
        }
        tok, _ = s.NextToken()
    }

    s.ConsumeToken()
    sexpr = ArrayToList(cells)
    return
}

func newSymbol(s *MyTokenizer, lit string) (sym *Data, eof bool, err error) {
    s.ConsumeToken()
    sym, err = makeSymbol(lit)
    return
}

func parseExpression(s *MyTokenizer) (sexpr *Data, eof bool, err error) {
    for {
        tok, lit := s.NextToken()
        //        fmt.Printf("token: %d\n", tok)
        switch tok {
        case EOF:
            eof = true
            err = nil
            return
        case COMMENT:
            s.ConsumeToken()
            break
        case NUMBER:
            s.ConsumeToken()
            sexpr, err = makeNumber(lit)
            return
        case HEXNUMBER:
            s.ConsumeToken()
            sexpr, err = makeHexNumber(lit)
            return
        case STRING:
            s.ConsumeToken()
            sexpr, err = makeString(lit)
            return
        case LPAREN:
            s.ConsumeToken()
            sexpr, eof, err = parseConsCell(s)
            return
        case SYMBOL:
            s.ConsumeToken()
            sexpr, err = makeSymbol(lit)
            return
        case FALSE:
            s.ConsumeToken()
            sexpr = False
            return
        case TRUE:
            s.ConsumeToken()
            sexpr = True
            return
        case QUOTE:
            s.ConsumeToken()
            sexpr, eof, err = parseExpression(s)
            if sexpr != nil {
                sexpr = Cons(SymbolWithName("quote"), Cons(sexpr, nil))
            }
            return
        case ILLEGAL:
            s.ConsumeToken()
            return
        default:
            s.ConsumeToken()
            sexpr, err = makeSymbol(lit)
            return
        }
    }
}

func Parse(src string) (sexpr *Data, err error) {
    s := NewMyTokenizer(src)
    sexpr, _, err = parseExpression(s)
    return
}

func ReadFile(filename string) (s string, err error) {
    fin, err := os.Open(filename)
    if err != nil {
        return
    }
    defer fin.Close()

    contents := make([]byte, 8192)
    _, err = fin.Read(contents)
    if err != nil {
        return
    }

    s = string(contents)
    return
}

func ProcessFile(filename string) (result *Data, err error) {
    src, err := ReadFile(filename)
    if err != nil {
        return
    }
    s := NewMyTokenizer(src)
    var sexpr *Data
    var eof bool
    for {
        sexpr, eof, err = parseExpression(s)
        if err != nil {
            return
        }
        if eof {
            return
        }
        if NilP(sexpr) {
            return
        }
        result, err = Eval(sexpr, Global)
        if err != nil {
            return
        }
    }
    return
}
