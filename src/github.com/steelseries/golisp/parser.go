// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file impliments the parser

package golisp

import (
    "errors"
    "fmt"
    "go/token"
    "unicode"
)

func makeNumber(str string) (n *Data, err error) {
    var i int
    _, err = fmt.Sscanf(str, "%d", &i)
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

func parseBoolean(s *Tokenizer) (b *Data, err error) {
    _, lit := s.NextToken()
    s.ConsumeToken()
    if lit[0] == 't' {
        return True, nil
    } else if lit[0] == 'f' {
        return False, nil
    } else {
        return nil, errors.New(fmt.Sprintf("#%s is not a legal boolean constant.", lit))
    }
}

func parseConsCell(s *Tokenizer) (sexpr *Data, eof bool, err error) {
    tok, _ := s.NextToken()
    if tok == token.RPAREN {
        s.ConsumeToken()
        sexpr = nil
        return
    }

    var car *Data
    var cdr *Data
    cells := make([]*Data, 0, 10)
    for tok != token.RPAREN {
        if tok == token.PERIOD {
            s.ConsumeToken()
            cdr, eof, err = parseExpression(s)
            if eof || err != nil {
                return
            }
            tok, _ = s.NextToken()
            if tok != token.RPAREN {
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

func isBooleanConstant(str string) bool {
    return str[0] == '#'
}

func isLispIdent(str string) bool {
    for _, c := range str {
        if !(unicode.IsLetter(c) || unicode.IsDigit(c) || c == '!' || c == '?' || c == '-') {
            return false
        }
    }
    return true
}

func newSymbol(s *Tokenizer, lit string) (sym *Data, eof bool, err error) {
    s.ConsumeToken()
    sym, err = makeSymbol(lit)
    return
}

func parseExpression(s *Tokenizer) (sexpr *Data, eof bool, err error) {
    for {
        tok, lit := s.NextToken()
        switch tok {
        case token.EOF:
            {
                eof = true
                err = nil
                return
            }
        case token.COMMENT:
            {
                s.ConsumeToken()
                break
            }
        case token.INT:
            {
                s.ConsumeToken()
                sexpr, err = makeNumber(lit)
                return
            }
        case token.STRING:
            {
                s.ConsumeToken()
                sexpr, err = makeString(lit)
                return
            }
        case token.LPAREN:
            {
                s.ConsumeToken()
                sexpr, eof, err = parseConsCell(s)
                return
            }
        case token.IDENT:
            {
                s.ConsumeToken()
                sexpr, err = makeSymbol(lit)
                return
            }
        case token.ADD:
            return newSymbol(s, "+")
        case token.SUB:
            return newSymbol(s, "-")
        case token.MUL:
            return newSymbol(s, "*")
        case token.QUO:
            return newSymbol(s, "/")
        case token.REM:
            return newSymbol(s, "%")
        case token.LSS:
            return newSymbol(s, "<")
        case token.GTR:
            return newSymbol(s, ">")
        case token.EQL:
            return newSymbol(s, "==")
        case token.NOT:
            return newSymbol(s, "!")
        case token.NEQ:
            return newSymbol(s, "!=")
        case token.LEQ:
            return newSymbol(s, "<=")
        case token.GEQ:
            return newSymbol(s, ">=")
        case token.IF:
            return newSymbol(s, "if")
        case token.MAP:
            return newSymbol(s, "map")
        case token.VAR:
            return newSymbol(s, "var")
        case token.ILLEGAL:
            {
                s.ConsumeToken()
                if isBooleanConstant(lit) {
                    sexpr, err = parseBoolean(s)
                } else {
                    println("Illegal symbol: `" + lit + "`")
                    err = errors.New("Illegal symbol: `" + lit + "`")
                }
                return
            }
        default:
            {
                s.ConsumeToken()
                if lit[0] == '\'' {
                    quoteSymbol := SymbolWithName("quote")
                    var value *Data
                    value, err = Parse(lit[1:])
                    if err != nil {
                        return
                    }
                    sexpr = Cons(quoteSymbol, Cons(value, nil))
                } else {
                    sexpr, err = makeSymbol(lit)
                }
                return
            }
        }
    }
}

func Parse(src string) (sexpr *Data, err error) {
    s := NewTokenizer(src)
    sexpr, _, err = parseExpression(s)
    return
}
