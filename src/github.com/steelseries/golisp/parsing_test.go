package golisp

import (
    "go/scanner"
    "go/token"
    . "launchpad.net/gocheck"
    "testing"
)

func Test(t *testing.T) { TestingT(t) }

type ParsingSuite struct{}

var _ = Suite(&ParsingSuite{})

func makeScanner(src string) scanner.Scanner {
    var s scanner.Scanner
    fset := token.NewFileSet()
    file := fset.AddFile("", fset.Base(), len(src))
    s.Init(file, []byte(src), nil, scanner.ScanComments)
    return s
}

// Atoms

func (s *ParsingSuite) TestNumber(c *C) {
    initScanner("5")
    sexpr, _, err := parseExpression()
    c.Assert(err, IsNil)
    c.Assert(sexpr, FitsTypeOf, NumberWithValue(5))
    c.Assert(sexpr.IntValue(), Equals, 5)
}

func (s *ParsingSuite) TestAnotherNumber(c *C) {
    initScanner("476")
    sexpr, _, err := parseExpression()
    c.Assert(err, IsNil)
    c.Assert(sexpr, FitsTypeOf, NumberWithValue(5))
    c.Assert(sexpr.IntValue(), Equals, 476)
}

func (s *ParsingSuite) TestString(c *C) {
    initScanner(`"test"`)
    sexpr, _, err := parseExpression()
    c.Assert(err, IsNil)
    c.Assert(sexpr, FitsTypeOf, StringWithValue("test"))
    c.Assert(sexpr.StringValue(), Equals, "test")
}

func (s *ParsingSuite) TestAnotherString(c *C) {
    initScanner(`"Lots Of Stylish Parentheses"`)
    sexpr, _, err := parseExpression()
    c.Assert(err, IsNil)
    c.Assert(sexpr, FitsTypeOf, StringWithValue("test"))
    c.Assert(sexpr.StringValue(), Equals, "Lots Of Stylish Parentheses")
}

func (s *ParsingSuite) TestBooleanTrue(c *C) {
    initScanner("#t")
    sexpr, _, err := parseExpression()
    c.Assert(err, IsNil)
    c.Assert(sexpr, FitsTypeOf, True)
    c.Assert(sexpr.BooleanValue(), Equals, true)
}

func (s *ParsingSuite) TestBooleanFalse(c *C) {
    initScanner("#f")
    sexpr, _, err := parseExpression()
    c.Assert(err, IsNil)
    c.Assert(sexpr, FitsTypeOf, True)
    c.Assert(sexpr.BooleanValue(), Equals, false)
}

func (s *ParsingSuite) TestBooleanAnythingElseIsFalse(c *C) {
    initScanner("#w")
    sexpr, _, err := parseExpression()
    c.Assert(err, IsNil)
    c.Assert(sexpr, FitsTypeOf, True)
    c.Assert(sexpr.BooleanValue(), Equals, false)
}

func (s *ParsingSuite) TestSymbol(c *C) {
    initScanner("test")
    sexpr, _, err := parseExpression()
    c.Assert(err, IsNil)
    c.Assert(sexpr, FitsTypeOf, SymbolWithName("test"))
    c.Assert(sexpr.IdentifierValue(), Equals, "test")
}

func (s *ParsingSuite) TestAnotherSymbol(c *C) {
    initScanner("defun")
    sexpr, _, err := parseExpression()
    c.Assert(err, IsNil)
    c.Assert(sexpr, FitsTypeOf, SymbolWithName("test"))
    c.Assert(sexpr.IdentifierValue(), Equals, "defun")
}

// compound structures

func (s *ParsingSuite) TestNil(c *C) {
    initScanner("()")
    sexpr, _, err := parseExpression()
    c.Assert(err, IsNil)
    c.Assert(sexpr, FitsTypeOf, Nil)
}

func (s *ParsingSuite) TestNumberCar(c *C) {
    initScanner("(1)")
    sexpr, _, err := parseExpression()
    c.Assert(err, IsNil)
    c.Assert(sexpr, FitsTypeOf, EmptyCons())
    c.Assert(sexpr.Head(), FitsTypeOf, NumberWithValue(0))
    c.Assert(sexpr.Head().IntValue(), Equals, 1)
}

func (s *ParsingSuite) TestStringCar(c *C) {
    initScanner(`("hello")`)
    sexpr, _, err := parseExpression()
    c.Assert(err, IsNil)
    c.Assert(sexpr, FitsTypeOf, EmptyCons())
    c.Assert(sexpr.Head(), FitsTypeOf, StringWithValue(""))
    c.Assert(sexpr.Head().StringValue(), Equals, "hello")
}

func (s *ParsingSuite) Test2ElementListCar(c *C) {
    initScanner("(1 2)")
    sexpr, _, err := parseExpression()
    c.Assert(err, IsNil)
    c.Assert(sexpr, FitsTypeOf, EmptyCons())
    c.Assert(sexpr.Head(), FitsTypeOf, NumberWithValue(0))
    c.Assert(sexpr.Head().IntValue(), Equals, 1)
    c.Assert(sexpr.Tail().Head(), FitsTypeOf, NumberWithValue(0))
    c.Assert(sexpr.Tail().Head().IntValue(), Equals, 2)
    c.Assert(sexpr.Tail().Tail(), Equals, Nil)
}
