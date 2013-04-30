// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file impliments the parser

package golisp

import (
    "errors"
    "fmt"
    "go/scanner"
    "go/token"
    "unicode"
)

var lookahead_token token.Token = -1
var lookahead_lit = ""
var s scanner.Scanner

func initScanner(src string) {
    fset := token.NewFileSet()
    file := fset.AddFile("", fset.Base(), len(src))
    s.Init(file, []byte(src), nil, scanner.ScanComments)
    consumeToken()
}

func nextToken() (tok token.Token, lit string) {
    return lookahead_token, lookahead_lit
}

func consumeToken() {
    _, lookahead_token, lookahead_lit = s.Scan()
}

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
    s = Intern(str)
    return
}

func parseBoolean() (b *Data, err error) {
    _, lit := nextToken()
    consumeToken()
    if lit[0] == 't' {
        return True, nil
    } else if lit[0] == 'f' {
        return False, nil
    } else {
        return nil, errors.New(fmt.Sprintf("#%s is not a legal boolean constant.", lit))
    }
}

func parseConsCell() (sexpr *Data, eof bool, err error) {
    tok, _ := nextToken()
    if tok == token.RPAREN {
        consumeToken()
        sexpr = nil
        return
    }

    var car *Data
    var cdr *Data
    cells := make([]*Data, 0, 10)
    for tok != token.RPAREN {
        if tok == token.PERIOD {
            consumeToken()
            cdr, eof, err = parseExpression()
            if eof || err != nil {
                return
            }
            tok, _ = nextToken()
            if tok != token.RPAREN {
                err = errors.New("Expected ')'")
                return
            }
            consumeToken()
            sexpr = arrayToListWithTail(cells, cdr)
            return
        } else {
            car, eof, err = parseExpression()
            if eof {
                err = errors.New("Unexpected EOF (expected closing parenthesis)")
                return
            }
            if err != nil {
                return
            }
            cells = append(cells, car)
        }
        tok, _ = nextToken()
    }

    consumeToken()
    sexpr = arrayToList(cells)
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

func newSymbol(lit string) (sym *Data, eof bool, err error) {
    consumeToken()
    sym, err = makeSymbol(lit)
    return
}

func parseExpression() (sexpr *Data, eof bool, err error) {
    for {
        tok, lit := nextToken()
        switch tok {
        case token.EOF:
            {
                eof = true
                err = nil
                return
            }
        case token.COMMENT:
            {
                consumeToken()
                break
            }
        case token.INT:
            {
                consumeToken()
                sexpr, err = makeNumber(lit)
                return
            }
        case token.STRING:
            {
                consumeToken()
                sexpr, err = makeString(lit)
                return
            }
        case token.LPAREN:
            {
                consumeToken()
                sexpr, eof, err = parseConsCell()
                return
            }
        case token.IDENT:
            {
                consumeToken()
                sexpr, err = makeSymbol(lit)
                return
            }
        case token.ADD:
            return newSymbol("+")
        case token.SUB:
            return newSymbol("-")
        case token.MUL:
            return newSymbol("*")
        case token.QUO:
            return newSymbol("/")
        case token.REM:
            return newSymbol("%")
        case token.LSS:
            return newSymbol("<")
        case token.GTR:
            return newSymbol(">")
        case token.EQL:
            return newSymbol("==")
        case token.NOT:
            return newSymbol("!")
        case token.NEQ:
            return newSymbol("!=")
        case token.LEQ:
            return newSymbol("<=")
        case token.GEQ:
            return newSymbol(">=")
        case token.IF:
            return newSymbol("if")
        case token.MAP:
            return newSymbol("map")
        case token.VAR:
            return newSymbol("var")
        case token.ILLEGAL:
            {
                consumeToken()
                if isBooleanConstant(lit) {
                    sexpr, err = parseBoolean()
                } else if isLispIdent(lit) {
                    sexpr, err = makeSymbol(lit)
                } else {
                    err = errors.New("Illegal symbol: `" + lit + "`")
                }
                return
            }
        default:
            {
                consumeToken()
                sexpr, err = makeSymbol(lit)
                return
            }
        }
    }
}

func Parse(src string) (sexpr *Data, err error) {
    initScanner(src)
    sexpr, _, err = parseExpression()
    return
}
