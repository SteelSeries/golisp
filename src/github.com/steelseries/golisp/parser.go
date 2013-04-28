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

func makeNumber(str string) (n Number, err error) {
    var i int
    _, err = fmt.Sscanf(str, "%d", &i)
    if err != nil {
        return
    }
    n = NumberWithValue(i)
    return
}

func makeString(str string) (s String, err error) {
    s = StringWithValue(str[1 : len(str)-1])
    return
}

func makeSymbol(str string) (s Symbol, err error) {
    s = SymbolWithName(str)
    return
}

func parseBoolean() (b Boolean, err error) {
    _, lit := nextToken()
    consumeToken()
    if lit[0] == 't' {
        return True, nil
    } else {
        return False, nil
    }
}

func parseConsCell() (sexpr Expression, eof bool, err error) {
    println("ParseConsCell")
    tok, _ := nextToken()
    if tok == token.RPAREN {
        consumeToken()
        println("Found RPAREN")
        sexpr = Nil
        return
    }

    var car Expression
    cells := make([]Expression, 0, 10)
    for tok != token.RPAREN {
        car, eof, err = parseExpression()
        if eof {
            err = errors.New("Unexpected EOF (expected closing parenthesis)")
            return
        }
        if err != nil {
            return
        }
        fmt.Printf("%v\n", car)
        cells = append(cells, car)
        fmt.Printf("%v\n", cells)
        tok, _ = nextToken()
    }

    println("Found RPAREN")
    sexpr = arrayToList(cells)
    return
}

func isBooleanConstant(str string) bool {
    return str[0] == '#'
}

func isLispIdent(str string) bool {
    for _, c := range str {
        if !(unicode.IsLetter(c) || unicode.IsDigit(c) || c == '!' || c == '?') {
            return false
        }
    }
    return true
}

func parseExpression() (sexpr Expression, eof bool, err error) {
    println("..ParseExpression")
    for {
        tok, lit := nextToken()
        switch tok {
        case token.EOF:
            {
                consumeToken()
                println("..Found EOF")
                eof = true
                err = nil
                return
            }
        case token.COMMENT:
            {
                consumeToken()
                println("..Found COMMENT")
                break
            }
        case token.INT:
            {
                consumeToken()
                println("..Found INT")
                sexpr, err = makeNumber(lit)
                return
            }
        case token.STRING:
            {
                consumeToken()
                println("..Found STRING")
                sexpr, err = makeString(lit)
                return
            }
        case token.IDENT:
            {
                consumeToken()
                println("..Found IDENT")
                sexpr, err = makeSymbol(lit)
                return
            }
        case token.ILLEGAL:
            {
                consumeToken()
                println("..Found ILLEGAL")
                if isBooleanConstant(lit) {
                    sexpr, err = parseBoolean()
                } else if isLispIdent(lit) {
                    sexpr, err = makeSymbol(lit)
                } else {
                    err = errors.New("Illegal symbol: `" + lit + "`")
                }
                return
            }
        case token.LPAREN:
            {
                consumeToken()
                println("..Found LPAREN")
                sexpr, eof, err = parseConsCell()
                return
            }
        }
    }
}

func Parse(src string) (sexprs Expression, err error) {
    initScanner(src)
    resultSexprs := make([]Expression, 0, 2)
    nextSexpr, eof, err := parseExpression()
    if err != nil {
        return
    }
    for !eof {
        resultSexprs = append(resultSexprs, nextSexpr)
        nextSexpr, eof, err = parseExpression()
        if eof {
            break
        }
        if err != nil {
            return
        }
    }
    return arrayToList(resultSexprs), nil
}
