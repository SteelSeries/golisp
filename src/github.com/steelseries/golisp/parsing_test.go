package golisp

import (
    . "launchpad.net/gocheck"
    "testing"
)

func Test(t *testing.T) { TestingT(t) }

type ParsingSuite struct{}

var _ = Suite(&ParsingSuite{})

// Atoms

func (s *ParsingSuite) TestNumber(c *C) {
    sexpr, err := Parse("5")
    c.Assert(err, IsNil)
    c.Assert(TypeOf(sexpr), Equals, NumberType)
    c.Assert(IntValue(sexpr), Equals, 5)
}

func (s *ParsingSuite) TestAnotherNumber(c *C) {
    sexpr, err := Parse("476")
    c.Assert(err, IsNil)
    c.Assert(TypeOf(sexpr), FitsTypeOf, NumberType)
    c.Assert(IntValue(sexpr), Equals, 476)
}

func (s *ParsingSuite) TestString(c *C) {
    sexpr, err := Parse(`"test"`)
    c.Assert(err, IsNil)
    c.Assert(TypeOf(sexpr), Equals, StringType)
    c.Assert(StringValue(sexpr), Equals, "test")
}

func (s *ParsingSuite) TestAnotherString(c *C) {
    sexpr, err := Parse(`"Lots Of Stylish Parentheses"`)
    c.Assert(err, IsNil)
    c.Assert(TypeOf(sexpr), Equals, StringType)
    c.Assert(StringValue(sexpr), Equals, "Lots Of Stylish Parentheses")
}

func (s *ParsingSuite) TestBooleanTrue(c *C) {
    sexpr, err := Parse("#t")
    c.Assert(err, IsNil)
    c.Assert(TypeOf(sexpr), Equals, BooleanType)
    c.Assert(BooleanValue(sexpr), Equals, true)
}

func (s *ParsingSuite) TestBooleanFalse(c *C) {
    sexpr, err := Parse("#f")
    c.Assert(err, IsNil)
    c.Assert(TypeOf(sexpr), Equals, BooleanType)
    c.Assert(BooleanValue(sexpr), Equals, false)
}

func (s *ParsingSuite) TestBooleanAnythingElseIsFalse(c *C) {
    _, err := Parse("#w")
    c.Assert(err, NotNil)
}

func (s *ParsingSuite) TestSymbol(c *C) {
    sexpr, err := Parse("test")
    c.Assert(err, IsNil)
    c.Assert(TypeOf(sexpr), Equals, SymbolType)
    c.Assert(StringValue(sexpr), Equals, "test")
}

func (s *ParsingSuite) TestAnotherSymbol(c *C) {
    sexpr, err := Parse("defun")
    c.Assert(err, IsNil)
    c.Assert(TypeOf(sexpr), Equals, SymbolType)
    c.Assert(StringValue(sexpr), Equals, "defun")
}

// compound structures

func (s *ParsingSuite) TestNil(c *C) {
    sexpr, err := Parse("()")
    c.Assert(err, IsNil)
    c.Assert(sexpr, IsNil)
}

func (s *ParsingSuite) TestNumberCar(c *C) {
    sexpr, err := Parse("(1)")
    c.Assert(err, IsNil)
    c.Assert(TypeOf(sexpr), Equals, ConsCellType)
    c.Assert(TypeOf(Car(sexpr)), Equals, NumberType)
    c.Assert(IntValue(Car(sexpr)), Equals, 1)
}

func (s *ParsingSuite) TestStringCar(c *C) {
    sexpr, err := Parse(`("hello")`)
    c.Assert(err, IsNil)
    c.Assert(TypeOf(sexpr), Equals, ConsCellType)
    c.Assert(TypeOf(Car(sexpr)), Equals, StringType)
    c.Assert(StringValue(Car(sexpr)), Equals, "hello")
}

func (s *ParsingSuite) Test2ElementList(c *C) {
    sexpr, err := Parse("(1 2)")
    c.Assert(err, IsNil)
    c.Assert(sexpr, FitsTypeOf, EmptyCons())
    c.Assert(TypeOf(Car(sexpr)), Equals, NumberType)
    c.Assert(IntValue(Car(sexpr)), Equals, 1)
    c.Assert(TypeOf(Car(Cdr(sexpr))), Equals, NumberType)
    c.Assert(IntValue(Car(Cdr(sexpr))), Equals, 2)
    c.Assert(Cdr(Cdr(sexpr)), IsNil)
}

func (s *ParsingSuite) TestNestedList(c *C) {
    sexpr, err := Parse("(1 (2 3) 4)")
    c.Assert(err, IsNil)
    c.Assert(TypeOf(sexpr), Equals, ConsCellType)

    c.Assert(TypeOf(Car(sexpr)), Equals, NumberType)
    c.Assert(IntValue(Car(sexpr)), Equals, 1)

    c.Assert(TypeOf(Cadr(sexpr)), Equals, ConsCellType)

    c.Assert(TypeOf(Caadr(sexpr)), Equals, NumberType)
    c.Assert(IntValue(Caadr(sexpr)), Equals, 2)

    c.Assert(TypeOf(Cadadr(sexpr)), Equals, NumberType)
    c.Assert(IntValue(Cadadr(sexpr)), Equals, 3)

    c.Assert(TypeOf(Caddr(sexpr)), Equals, NumberType)
    c.Assert(IntValue(Caddr(sexpr)), Equals, 4)

    c.Assert(Cdr(Cddr(sexpr)), IsNil)
}

func (s *ParsingSuite) TestDottedPair(c *C) {
    sexpr, err := Parse("(1 . 2)")
    c.Assert(err, IsNil)
    c.Assert(TypeOf(sexpr), Equals, ConsCellType)

    c.Assert(TypeOf(Car(sexpr)), Equals, NumberType)
    c.Assert(IntValue(Car(sexpr)), Equals, 1)
    c.Assert(TypeOf(Cdr(sexpr)), Equals, NumberType)
    c.Assert(IntValue(Cdr(sexpr)), Equals, 2)
}

func (s *ParsingSuite) TestPrimitive(c *C) {
    sexpr, err := Parse("(+ 1 2)")
    c.Assert(err, IsNil)
    c.Assert(TypeOf(sexpr), Equals, ConsCellType)

    c.Assert(TypeOf(Car(sexpr)), Equals, SymbolType)
    c.Assert(StringValue(Car(sexpr)), Equals, "+")

    c.Assert(TypeOf(Cadr(sexpr)), Equals, NumberType)
    c.Assert(IntValue(Cadr(sexpr)), Equals, 1)

    c.Assert(TypeOf(Caddr(sexpr)), Equals, NumberType)
    c.Assert(IntValue(Caddr(sexpr)), Equals, 2)
}
