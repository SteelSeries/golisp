// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file tests the parser.

package golisp

import (
	"testing"

	. "gopkg.in/check.v1"
)

func Test(t *testing.T) { TestingT(t) }

type ParsingSuite struct{}

var _ = Suite(&ParsingSuite{})

// Atoms

func (s *ParsingSuite) TestInteger(c *C) {
	sexpr, err := Parse("5")
	c.Assert(err, IsNil)
	c.Assert(sexpr, NotNil)
	c.Assert(int(TypeOf(sexpr)), Equals, IntegerType)
	c.Assert(IntegerValue(sexpr), Equals, int64(5))
}

func (s *ParsingSuite) TestAnotherInteger(c *C) {
	sexpr, err := Parse("476")
	c.Assert(err, IsNil)
	c.Assert(sexpr, NotNil)
	c.Assert(int(TypeOf(sexpr)), FitsTypeOf, IntegerType)
	c.Assert(IntegerValue(sexpr), Equals, int64(476))
}

func (s *ParsingSuite) TestNegaitveInteger(c *C) {
	sexpr, err := Parse("-5")
	c.Assert(err, IsNil)
	c.Assert(sexpr, NotNil)
	c.Assert(int(TypeOf(sexpr)), Equals, IntegerType)
	c.Assert(IntegerValue(sexpr), Equals, int64(-5))
}

func (s *ParsingSuite) TestHexInteger(c *C) {
	sexpr, err := Parse("0xa5")
	c.Assert(err, IsNil)
	c.Assert(sexpr, NotNil)
	c.Assert(int(TypeOf(sexpr)), Equals, IntegerType)
	c.Assert(IntegerValue(sexpr), Equals, int64(165))
}

func (s *ParsingSuite) TestProperHexInteger(c *C) {
	sexpr, err := Parse("#xa5")
	c.Assert(err, IsNil)
	c.Assert(sexpr, NotNil)
	c.Assert(int(TypeOf(sexpr)), Equals, IntegerType)
	c.Assert(IntegerValue(sexpr), Equals, int64(165))
}

func (s *ParsingSuite) TestBinaryInteger(c *C) {
	sexpr, err := Parse("#b1010")
	c.Assert(err, IsNil)
	c.Assert(sexpr, NotNil)
	c.Assert(int(TypeOf(sexpr)), Equals, IntegerType)
	c.Assert(IntegerValue(sexpr), Equals, int64(10))
}

func (s *ParsingSuite) TestFloat(c *C) {
	sexpr, err := Parse("12.345")
	c.Assert(err, IsNil)
	c.Assert(sexpr, NotNil)
	c.Assert(int(TypeOf(sexpr)), Equals, FloatType)
	c.Assert(FloatValue(sexpr), Equals, float32(12.345))
}

func (s *ParsingSuite) TestNegativeFloat(c *C) {
	sexpr, err := Parse("-12.345")
	c.Assert(err, IsNil)
	c.Assert(sexpr, NotNil)
	c.Assert(int(TypeOf(sexpr)), Equals, FloatType)
	c.Assert(FloatValue(sexpr), Equals, float32(-12.345))
}

func (s *ParsingSuite) TestUppercaseHexInteger(c *C) {
	sexpr, err := Parse("0xA5")
	c.Assert(err, IsNil)
	c.Assert(sexpr, NotNil)
	c.Assert(int(TypeOf(sexpr)), Equals, IntegerType)
	c.Assert(IntegerValue(sexpr), Equals, int64(165))
}

func (s *ParsingSuite) TestMixedCaseHexInteger(c *C) {
	sexpr, err := Parse("0xAf")
	c.Assert(err, IsNil)
	c.Assert(sexpr, NotNil)
	c.Assert(int(TypeOf(sexpr)), Equals, IntegerType)
	c.Assert(IntegerValue(sexpr), Equals, int64(175))
}

func (s *ParsingSuite) TestString(c *C) {
	sexpr, err := Parse(`"test"`)
	c.Assert(err, IsNil)
	c.Assert(sexpr, NotNil)
	c.Assert(int(TypeOf(sexpr)), Equals, StringType)
	c.Assert(StringValue(sexpr), Equals, "test")
}

func (s *ParsingSuite) TestAnotherString(c *C) {
	sexpr, err := Parse(`"Lots Of Stylish Parentheses"`)
	c.Assert(err, IsNil)
	c.Assert(sexpr, NotNil)
	c.Assert(int(TypeOf(sexpr)), Equals, StringType)
	c.Assert(StringValue(sexpr), Equals, "Lots Of Stylish Parentheses")
}

func (s *ParsingSuite) TestBooleanTrue(c *C) {
	sexpr, err := Parse("#t")
	c.Assert(err, IsNil)
	c.Assert(sexpr, NotNil)
	c.Assert(int(TypeOf(sexpr)), Equals, BooleanType)
	c.Assert(BooleanValue(sexpr), Equals, true)
}

func (s *ParsingSuite) TestBooleanFalse(c *C) {
	sexpr, err := Parse("#f")
	c.Assert(err, IsNil)
	c.Assert(sexpr, NotNil)
	c.Assert(int(TypeOf(sexpr)), Equals, BooleanType)
	c.Assert(BooleanValue(sexpr), Equals, false)
}

func (s *ParsingSuite) TestBooleanAnythingElseIsIllegal(c *C) {
	_, err := Parse("#w")
	c.Assert(err, NotNil)
}

func (s *ParsingSuite) TestSymbol(c *C) {
	sexpr, err := Parse("test")
	c.Assert(err, IsNil)
	c.Assert(sexpr, NotNil)
	c.Assert(int(TypeOf(sexpr)), Equals, SymbolType)
	c.Assert(StringValue(sexpr), Equals, "test")
}

func (s *ParsingSuite) TestAnotherSymbol(c *C) {
	sexpr, err := Parse("defun")
	c.Assert(err, IsNil)
	c.Assert(sexpr, NotNil)
	c.Assert(int(TypeOf(sexpr)), Equals, SymbolType)
	c.Assert(StringValue(sexpr), Equals, "defun")
}

func (s *ParsingSuite) TestSymbolWithUnderscores(c *C) {
	sexpr, err := Parse("_test_1")
	c.Assert(err, IsNil)
	c.Assert(sexpr, NotNil)
	c.Assert(int(TypeOf(sexpr)), Equals, SymbolType)
	c.Assert(StringValue(sexpr), Equals, "_test_1")
}

// compound structures

func (s *ParsingSuite) TestNil(c *C) {
	sexpr, err := Parse("()")
	c.Assert(err, IsNil)
	c.Assert(NilP(sexpr), Equals, true)
}

func (s *ParsingSuite) TestIntegerCar(c *C) {
	sexpr, err := Parse("(1)")
	c.Assert(err, IsNil)
	c.Assert(sexpr, NotNil)
	c.Assert(int(TypeOf(sexpr)), Equals, ConsCellType)
	c.Assert(int(TypeOf(Car(sexpr))), Equals, IntegerType)
	c.Assert(IntegerValue(Car(sexpr)), Equals, int64(1))
}

func (s *ParsingSuite) TestStringCar(c *C) {
	sexpr, err := Parse(`("hello")`)
	c.Assert(err, IsNil)
	c.Assert(sexpr, NotNil)
	c.Assert(int(TypeOf(sexpr)), Equals, ConsCellType)
	c.Assert(int(TypeOf(Car(sexpr))), Equals, StringType)
	c.Assert(StringValue(Car(sexpr)), Equals, "hello")
}

func (s *ParsingSuite) Test2ElementList(c *C) {
	sexpr, err := Parse("(1 2)")
	c.Assert(err, IsNil)
	c.Assert(sexpr, NotNil)
	c.Assert(sexpr, FitsTypeOf, EmptyCons())
	c.Assert(int(TypeOf(Car(sexpr))), Equals, IntegerType)
	c.Assert(IntegerValue(Car(sexpr)), Equals, int64(1))
	c.Assert(int(TypeOf(Car(Cdr(sexpr)))), Equals, IntegerType)
	c.Assert(IntegerValue(Car(Cdr(sexpr))), Equals, int64(2))
	c.Assert(Cdr(Cdr(sexpr)), IsNil)
}

func (s *ParsingSuite) TestNestedList(c *C) {
	sexpr, err := Parse("(1 (2 3) 4)")
	c.Assert(err, IsNil)
	c.Assert(sexpr, NotNil)
	c.Assert(int(TypeOf(sexpr)), Equals, ConsCellType)

	c.Assert(int(TypeOf(Car(sexpr))), Equals, IntegerType)
	c.Assert(IntegerValue(Car(sexpr)), Equals, int64(1))

	c.Assert(int(TypeOf(Cadr(sexpr))), Equals, ConsCellType)

	c.Assert(int(TypeOf(WalkList(sexpr, "aad"))), Equals, IntegerType)
	c.Assert(IntegerValue(WalkList(sexpr, "aad")), Equals, int64(2))

	c.Assert(int(TypeOf(WalkList(sexpr, "adad"))), Equals, IntegerType)
	c.Assert(IntegerValue(WalkList(sexpr, "adad")), Equals, int64(3))

	c.Assert(int(TypeOf(Third(sexpr))), Equals, IntegerType)
	c.Assert(IntegerValue(Third(sexpr)), Equals, int64(4))

	c.Assert(Cdr(Cddr(sexpr)), IsNil)
}

func (s *ParsingSuite) TestDottedPair(c *C) {
	sexpr, err := Parse("(1 . 2)")
	c.Assert(err, IsNil)
	c.Assert(sexpr, NotNil)
	c.Assert(int(TypeOf(sexpr)), Equals, ConsCellType)

	c.Assert(int(TypeOf(Car(sexpr))), Equals, IntegerType)
	c.Assert(IntegerValue(Car(sexpr)), Equals, int64(1))
	c.Assert(int(TypeOf(Cdr(sexpr))), Equals, IntegerType)
	c.Assert(IntegerValue(Cdr(sexpr)), Equals, int64(2))
}

func (s *ParsingSuite) TestPrimitive(c *C) {
	sexpr, err := Parse("(+ 1 2)")
	c.Assert(err, IsNil)
	c.Assert(sexpr, NotNil)
	c.Assert(int(TypeOf(sexpr)), Equals, ConsCellType)

	c.Assert(int(TypeOf(Car(sexpr))), Equals, SymbolType)
	c.Assert(StringValue(Car(sexpr)), Equals, "+")

	c.Assert(int(TypeOf(Cadr(sexpr))), Equals, IntegerType)
	c.Assert(IntegerValue(Cadr(sexpr)), Equals, int64(1))

	c.Assert(int(TypeOf(Third(sexpr))), Equals, IntegerType)
	c.Assert(IntegerValue(Third(sexpr)), Equals, int64(2))
}

func (s *ParsingSuite) TestByteArray(c *C) {
	sexpr, err := Parse("[1 2]")
	c.Assert(err, IsNil)
	c.Assert(sexpr, NotNil)
	c.Assert(int(TypeOf(sexpr)), Equals, BoxedObjectType)
	c.Assert(ObjectType(sexpr), Equals, "[]byte")
	bytes := (*[]byte)(ObjectValue(sexpr))
	c.Assert(len(*bytes), Equals, 2)
	c.Assert((*bytes)[0], Equals, uint8(1))
	c.Assert((*bytes)[1], Equals, uint8(2))
}

func (s *ParsingSuite) TestEmptyByteArray(c *C) {
	sexpr, err := Parse("[]")
	c.Assert(err, IsNil)
	c.Assert(sexpr, NotNil)
	c.Assert(int(TypeOf(sexpr)), Equals, BoxedObjectType)
	c.Assert(ObjectType(sexpr), Equals, "[]byte")
	bytes := (*[]byte)(ObjectValue(sexpr))
	c.Assert(len(*bytes), Equals, 0)
}

func (s *ParsingSuite) TestQuote(c *C) {
	sexpr, err := Parse("'a")
	c.Assert(err, IsNil)
	c.Assert(sexpr, NotNil)
	c.Assert(int(TypeOf(sexpr)), Equals, ConsCellType)

	c.Assert(int(TypeOf(Car(sexpr))), Equals, SymbolType)
	c.Assert(StringValue(Car(sexpr)), Equals, "quote")

	c.Assert(int(TypeOf(Cadr(sexpr))), Equals, SymbolType)
	c.Assert(StringValue(Cadr(sexpr)), Equals, "a")
}

func (s *ParsingSuite) TestQuasiQuote(c *C) {
	sexpr, err := Parse("`a")
	c.Assert(err, IsNil)
	c.Assert(sexpr, NotNil)
	c.Assert(int(TypeOf(sexpr)), Equals, ConsCellType)

	c.Assert(int(TypeOf(Car(sexpr))), Equals, SymbolType)
	c.Assert(StringValue(Car(sexpr)), Equals, "quasiquote")

	c.Assert(int(TypeOf(Cadr(sexpr))), Equals, SymbolType)
	c.Assert(StringValue(Cadr(sexpr)), Equals, "a")
}

func (s *ParsingSuite) TestUnquote(c *C) {
	sexpr, err := Parse(",a")
	c.Assert(err, IsNil)
	c.Assert(sexpr, NotNil)
	c.Assert(int(TypeOf(sexpr)), Equals, ConsCellType)

	c.Assert(int(TypeOf(Car(sexpr))), Equals, SymbolType)
	c.Assert(StringValue(Car(sexpr)), Equals, "unquote")

	c.Assert(int(TypeOf(Cadr(sexpr))), Equals, SymbolType)
	c.Assert(StringValue(Cadr(sexpr)), Equals, "a")
}

func (s *ParsingSuite) TestUnquoteSplicing(c *C) {
	sexpr, err := Parse(",@a")
	c.Assert(err, IsNil)
	c.Assert(sexpr, NotNil)
	c.Assert(int(TypeOf(sexpr)), Equals, ConsCellType)

	c.Assert(int(TypeOf(Car(sexpr))), Equals, SymbolType)
	c.Assert(StringValue(Car(sexpr)), Equals, "unquote-splicing")

	c.Assert(int(TypeOf(Cadr(sexpr))), Equals, SymbolType)
	c.Assert(StringValue(Cadr(sexpr)), Equals, "a")
}

func (s *ParsingSuite) TestComment(c *C) {
	sexpr, err := Parse("; comment\n42")
	c.Assert(err, IsNil)
	c.Assert(sexpr, NotNil)
	c.Assert(int(TypeOf(sexpr)), Equals, IntegerType)
	c.Assert(IntegerValue(sexpr), Equals, int64(42))
}

func (s *ParsingSuite) TestParseAndEval(c *C) {
	result, err := ParseAndEval("(* 5 5)")
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, IntegerType)
	c.Assert(IntegerValue(result), Equals, int64(25))
}

func (s *ParsingSuite) BenchmarkParse(c *C) {
	c.ResetTimer()
	for i := 0; i < c.N; i++ {
		src, _ := ReadFile("tests/list_access_test.lsp")
		_, _ = ParseAndEval(src)
	}
}

func (s *ParsingSuite) Test_isNum(c *C) {
	c.Assert(isJustNum("0"), Equals, true)
	c.Assert(isJustNum("5"), Equals, true)
	c.Assert(isJustNum("9"), Equals, true)
	c.Assert(isJustNum("10"), Equals, true)
	c.Assert(isJustNum("124050"), Equals, true)

	c.Assert(isJustNum("_1"), Equals, false)
	c.Assert(isJustNum(">=1"), Equals, false)
	c.Assert(isJustNum("(1)"), Equals, false)
	c.Assert(isJustNum(""), Equals, false)
}

func (s *ParsingSuite) Test_isGTE(c *C) {
	c.Assert(isGTE(">=0"), Equals, true)
	c.Assert(isGTE(">=1"), Equals, true)
	c.Assert(isGTE(">=9"), Equals, true)
	c.Assert(isGTE(">=10"), Equals, true)

	c.Assert(isGTE("10"), Equals, false)
	c.Assert(isGTE(">="), Equals, false)
	c.Assert(isGTE("<=-"), Equals, false)
	c.Assert(isGTE(">=+"), Equals, false)
}

func (s *ParsingSuite) Test_isRange(c *C) {
	c.Assert(isRange("(1,1)"), Equals, true)
	c.Assert(isRange("(10,14)"), Equals, true)
	c.Assert(isRange("(1,00400)"), Equals, true)
	c.Assert(isRange("(999991,00400)"), Equals, true)

	c.Assert(isRange("()"), Equals, false)
	c.Assert(isRange("(,)"), Equals, false)
	c.Assert(isRange("(,00)"), Equals, false)
	c.Assert(isRange("(00,)"), Equals, false)
	c.Assert(isRange("(000)"), Equals, false)
	c.Assert(isRange("1234)"), Equals, false)
	c.Assert(isRange("(1234"), Equals, false)
}
