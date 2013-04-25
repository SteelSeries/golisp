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

func makeNumber(str string) (n Number, err error) {
    fmt.Printf("Making number from '%s'\n", str)
    var i int
    _, err = fmt.Sscanf(str, "%d", &i)
    if err != nil {
        return
    }
    n = NumberWithValue(i)
    return
}

func makeString(str string) (s String, err error) {
    s = StringWithValue(str)
    return
}

func makeSymbol(str string) (s Symbol, err error) {
    s = SymbolWithName(str)
    return
}

func parseBoolean(s *scanner.Scanner) (b Boolean, err error) {
    _, _, lit := (*s).Scan()
    if lit[0] == 't' {
        return True, nil
    } else {
        return False, nil
    }
}

func parseConsCell(s *scanner.Scanner) (sexpr Expression, eof bool, err error) {
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

func parseExpression(s *scanner.Scanner) (sexpr Expression, eof bool, err error) {
    for {
        _, tok, lit := (*s).Scan()
        fmt.Printf("Found token: '%s'\n", lit)
        switch tok {
        case token.EOF:
            {
                eof = true
                err = nil
                return
            }
        case token.COMMENT:
            break
        case token.INT:
            {
                sexpr, err = makeNumber(lit)
                return
            }
        case token.STRING:
            {
                sexpr, err = makeString(lit)
                return
            }
        case token.IDENT:
            {
                sexpr, err = makeSymbol(lit)
                return
            }
        case token.ILLEGAL:
            {
                if isBooleanConstant(lit) {
                    sexpr, err = parseBoolean(s)
                } else if isLispIdent(lit) {
                    sexpr, err = makeSymbol(lit)
                } else {
                    err = errors.New("Illegal symbol: `" + lit + "`")
                }
                return
            }
        case token.LPAREN:
            {
                sexpr, eof, err = parseConsCell(s)
                return
            }
        }
    }
}

func Parse(src string) (sexprs Expression, err error) {
    fmt.Printf("Parsing: '%s'\n", src)
    var s scanner.Scanner
    fset := token.NewFileSet()
    file := fset.AddFile("", fset.Base(), len(src))
    s.Init(file, []byte(src), nil, scanner.ScanComments)
    resultSexprs := make([]Expression, 0, 2)
    nextSexpr, eof, err := parseExpression(&s)
    if err != nil {
        return
    }
    for !eof {
        resultSexprs = append(resultSexprs, nextSexpr)
        nextSexpr, eof, err = parseExpression(&s)
        if eof {
            break
        }
        if err != nil {
            return
        }
    }
    return arrayToList(resultSexprs), nil
}
