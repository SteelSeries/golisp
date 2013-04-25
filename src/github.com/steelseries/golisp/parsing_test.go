package golisp

import (
    "go/scanner"
    "go/token"
    . "launchpad.net/gocheck"
    "testing"
)

func Test(t *testing.T) { TestingT(t) }

type AtomicParsingSuite struct{}

var _ = Suite(&AtomicParsingSuite{})

func makeScanner(src string) scanner.Scanner {
    var s scanner.Scanner
    fset := token.NewFileSet()
    file := fset.AddFile("", fset.Base(), len(src))
    s.Init(file, []byte(src), nil, scanner.ScanComments)
    return s
}

func (s *AtomicParsingSuite) TestNumber(c *C) {
    scanner := makeScanner("5")
    sexpr, _, err := parseExpression(&scanner)
    c.Assert(err, IsNil)
    c.Assert(sexpr, FitsTypeOf, NumberWithValue(5))
}

func (s *AtomicParsingSuite) TestString(c *C) {
    scanner := makeScanner(`"test"`)
    sexpr, _, err := parseExpression(&scanner)
    c.Assert(err, IsNil)
    c.Assert(sexpr, FitsTypeOf, StringWithValue("test"))
}

func (s *AtomicParsingSuite) TestBoolean(c *C) {
    scanner := makeScanner("#t")
    sexpr, _, err := parseExpression(&scanner)
    c.Assert(err, IsNil)
    c.Assert(sexpr, FitsTypeOf, True)
}

func (s *AtomicParsingSuite) TestSymbol(c *C) {
    scanner := makeScanner("test")
    sexpr, _, err := parseExpression(&scanner)
    c.Assert(err, IsNil)
    c.Assert(sexpr, FitsTypeOf, SymbolWithName("test"))
}
