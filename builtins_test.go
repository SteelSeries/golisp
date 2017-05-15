// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file tests built-in primitive functions.

package golisp

import (
	. "gopkg.in/check.v1"
)

type BuiltinsSuite struct {
}

var _ = Suite(&BuiltinsSuite{})

func (s *BuiltinsSuite) SetUpSuite(c *C) {
	InitLisp()
}

// Add

func (s *BuiltinsSuite) TestUnaryAdd(c *C) {
	code, _ := Parse("(+ 1)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, IntegerType)
	c.Assert(IntegerValue(result), Equals, int64(1))
}

func (s *BuiltinsSuite) TestBinaryAdd(c *C) {
	code, _ := Parse("(+ 1 2)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, IntegerType)
	c.Assert(IntegerValue(result), Equals, int64(3))
}

func (s *BuiltinsSuite) TestTrinaryAdd(c *C) {
	code, _ := Parse("(+ 1 2 3)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, IntegerType)
	c.Assert(IntegerValue(result), Equals, int64(6))
}

// Subtract

func (s *BuiltinsSuite) TestBinarySubtract(c *C) {
	code, _ := Parse("(- 2 1)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, IntegerType)
	c.Assert(IntegerValue(result), Equals, int64(1))
}

func (s *BuiltinsSuite) TestTrinarySubtract(c *C) {
	code, _ := Parse("(- 3 2 1)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, IntegerType)
	c.Assert(IntegerValue(result), Equals, int64(0))
}

// Mutiply

func (s *BuiltinsSuite) TestUnaryMultiply(c *C) {
	code, _ := Parse("(* 2)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, IntegerType)
	c.Assert(IntegerValue(result), Equals, int64(2))
}

func (s *BuiltinsSuite) TestBinaryMultiplty(c *C) {
	code, _ := Parse("(* 2 3)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, IntegerType)
	c.Assert(IntegerValue(result), Equals, int64(6))
}

func (s *BuiltinsSuite) TestTrinaryMultiply(c *C) {
	code, _ := Parse("(* 2 3 4)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, IntegerType)
	c.Assert(IntegerValue(result), Equals, int64(24))
}

// Quotient

func (s *BuiltinsSuite) TestUnaryQuotient(c *C) {
	code, _ := Parse("(/ 4)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, FloatType)
	c.Assert(FloatValue(result), Equals, float64(0.25))
}

func (s *BuiltinsSuite) TestBinaryQuotient(c *C) {
	code, _ := Parse("(/ 24 2)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, IntegerType)
	c.Assert(IntegerValue(result), Equals, int64(12))
}

func (s *BuiltinsSuite) TestTrinaryQuotient(c *C) {
	code, _ := Parse("(/ 24 2 3)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, IntegerType)
	c.Assert(IntegerValue(result), Equals, int64(4))
}

// compound

func (s *BuiltinsSuite) TestCompoundMath(c *C) {
	code, _ := Parse("(+ 1 (* 2 3))")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, IntegerType)
	c.Assert(IntegerValue(result), Equals, int64(7))
}

// %

func (s *BuiltinsSuite) TestRemainderWithNoArgs(c *C) {
	code, _ := Parse("(%)")
	_, err := Eval(code, Global)
	c.Assert(err, NotNil)
}

func (s *BuiltinsSuite) TestRemainderWithOneArg(c *C) {
	code, _ := Parse("(% 5)")
	_, err := Eval(code, Global)
	c.Assert(err, NotNil)
}

func (s *BuiltinsSuite) TestRemainder1(c *C) {
	code, _ := Parse("(% 5 2)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, IntegerType)
	c.Assert(IntegerValue(result), Equals, int64(1))
}

func (s *BuiltinsSuite) TestRemainder3(c *C) {
	code, _ := Parse("(% 7 4)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, IntegerType)
	c.Assert(IntegerValue(result), Equals, int64(3))
}

func (s *BuiltinsSuite) TestRemainder0(c *C) {
	code, _ := Parse("(% 7 7)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, IntegerType)
	c.Assert(IntegerValue(result), Equals, int64(0))
}

// <

func (s *BuiltinsSuite) TestLessThanWithNoArgs(c *C) {
	code, _ := Parse("(<)")
	_, err := Eval(code, Global)
	c.Assert(err, NotNil)
}

func (s *BuiltinsSuite) TestLessThanWithOneArg(c *C) {
	code, _ := Parse("(< 5)")
	_, err := Eval(code, Global)
	c.Assert(err, NotNil)
}

func (s *BuiltinsSuite) TestLessThanFalse(c *C) {
	code, _ := Parse("(< 5 2)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, BooleanType)
	c.Assert(BooleanValue(result), Equals, false)
}

func (s *BuiltinsSuite) TestLessThanTrue(c *C) {
	code, _ := Parse("(< 2 5)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, BooleanType)
	c.Assert(BooleanValue(result), Equals, true)
}

func (s *BuiltinsSuite) TestLessThanBoundry(c *C) {
	code, _ := Parse("(< 2 2)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, BooleanType)
	c.Assert(BooleanValue(result), Equals, false)
}

// >

func (s *BuiltinsSuite) TestGreaterThanWithNoArgs(c *C) {
	code, _ := Parse("(>)")
	_, err := Eval(code, Global)
	c.Assert(err, NotNil)
}

func (s *BuiltinsSuite) TestGreaterThanWithOneArg(c *C) {
	code, _ := Parse("(> 5)")
	_, err := Eval(code, Global)
	c.Assert(err, NotNil)
}

func (s *BuiltinsSuite) TestGreaterThanFalse(c *C) {
	code, _ := Parse("(> 2 5)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, BooleanType)
	c.Assert(BooleanValue(result), Equals, false)
}

func (s *BuiltinsSuite) TestGreaterThanTrue(c *C) {
	code, _ := Parse("(> 5 2)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, BooleanType)
	c.Assert(BooleanValue(result), Equals, true)
}

func (s *BuiltinsSuite) TestGreaterThanBoundry(c *C) {
	code, _ := Parse("(> 2 2)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, BooleanType)
	c.Assert(BooleanValue(result), Equals, false)
}

// ==

func (s *BuiltinsSuite) TestEqualToWithNoArgs(c *C) {
	code, _ := Parse("(==)")
	_, err := Eval(code, Global)
	c.Assert(err, NotNil)
}

func (s *BuiltinsSuite) TestEqualToWithOneArg(c *C) {
	code, _ := Parse("(== 5)")
	_, err := Eval(code, Global)
	c.Assert(err, NotNil)
}

func (s *BuiltinsSuite) TestEqualToFalse(c *C) {
	code, _ := Parse("(== 2 5)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, BooleanType)
	c.Assert(BooleanValue(result), Equals, false)
}

func (s *BuiltinsSuite) TestEqualToTrue(c *C) {
	code, _ := Parse("(== 2 2)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, BooleanType)
	c.Assert(BooleanValue(result), Equals, true)
}

// !=

func (s *BuiltinsSuite) TestNotEqualToWithNoArgs(c *C) {
	code, _ := Parse("(!=)")
	_, err := Eval(code, Global)
	c.Assert(err, NotNil)
}

func (s *BuiltinsSuite) TestNotEqualToWithOneArg(c *C) {
	code, _ := Parse("(!= 5)")
	_, err := Eval(code, Global)
	c.Assert(err, NotNil)
}

func (s *BuiltinsSuite) TestNotEqualToFalse(c *C) {
	code, _ := Parse("(!= 2 2)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, BooleanType)
	c.Assert(BooleanValue(result), Equals, false)
}

func (s *BuiltinsSuite) TestNotEqualToTrue(c *C) {
	code, _ := Parse("(!= 2 5)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, BooleanType)
	c.Assert(BooleanValue(result), Equals, true)
}

// <=

func (s *BuiltinsSuite) TestLessThanEqualWithNoArgs(c *C) {
	code, _ := Parse("(<=)")
	_, err := Eval(code, Global)
	c.Assert(err, NotNil)
}

func (s *BuiltinsSuite) TestLessThanEqualWithOneArg(c *C) {
	code, _ := Parse("(<= 5)")
	_, err := Eval(code, Global)
	c.Assert(err, NotNil)
}

func (s *BuiltinsSuite) TestLessThanEqualFalse(c *C) {
	code, _ := Parse("(<= 5 2)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, BooleanType)
	c.Assert(BooleanValue(result), Equals, false)
}

func (s *BuiltinsSuite) TestLessThanEqualTrue(c *C) {
	code, _ := Parse("(<= 2 5)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, BooleanType)
	c.Assert(BooleanValue(result), Equals, true)
}

func (s *BuiltinsSuite) TestLessThanEqualBoundry(c *C) {
	code, _ := Parse("(<= 2 2)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, BooleanType)
	c.Assert(BooleanValue(result), Equals, true)
}

// >=

func (s *BuiltinsSuite) TestGreaterThanEqualWithNoArgs(c *C) {
	code, _ := Parse("(>=)")
	_, err := Eval(code, Global)
	c.Assert(err, NotNil)
}

func (s *BuiltinsSuite) TestGreaterThanEqualWithOneArg(c *C) {
	code, _ := Parse("(>= 5)")
	_, err := Eval(code, Global)
	c.Assert(err, NotNil)
}

func (s *BuiltinsSuite) TestGreaterThanEqualFalse(c *C) {
	code, _ := Parse("(>= 2 5)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, BooleanType)
	c.Assert(BooleanValue(result), Equals, false)
}

func (s *BuiltinsSuite) TestGreaterThanEqualTrue(c *C) {
	code, _ := Parse("(>= 5 2)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, BooleanType)
	c.Assert(BooleanValue(result), Equals, true)
}

func (s *BuiltinsSuite) TestGreaterThanEqualBoundry(c *C) {
	code, _ := Parse("(>= 2 2)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, BooleanType)
	c.Assert(BooleanValue(result), Equals, true)
}

// !

func (s *BuiltinsSuite) TestNotWithNoArgs(c *C) {
	code, _ := Parse("(!)")
	_, err := Eval(code, Global)
	c.Assert(err, NotNil)
}

func (s *BuiltinsSuite) TestNotWithFalse(c *C) {
	code, _ := Parse("(! #f)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, BooleanType)
	c.Assert(BooleanValue(result), Equals, true)
}

func (s *BuiltinsSuite) TestNotWithTrue(c *C) {
	code, _ := Parse("(! #t)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, BooleanType)
	c.Assert(BooleanValue(result), Equals, false)
}

func (s *BuiltinsSuite) TestDefine(c *C) {
	code, _ := Parse("(define x 5)")
	Eval(code, Global)
	sym := SymbolWithName("x")
	v := Global.ValueOf(sym)
	c.Assert(v, NotNil)
	c.Assert(int(TypeOf(v)), Equals, IntegerType)
	c.Assert(IntegerValue(v), Equals, int64(5))
}

func (s *BuiltinsSuite) TestDefineAString(c *C) {
	code, _ := Parse(`(define x "a string")`)
	Eval(code, Global)
	sym := SymbolWithName("x")
	v := Global.ValueOf(sym)
	c.Assert(v, NotNil)
	c.Assert(int(TypeOf(v)), Equals, StringType)
	c.Assert(StringValue(v), Equals, "a string")
}

func (s *BuiltinsSuite) TestDefineAStringForIssue1(c *C) {
	code, _ := Parse(`(define some_string "I am a string")`)
	Eval(code, Global)
	sym := SymbolWithName("x")
	v := Global.ValueOf(sym)
	c.Assert(v, NotNil)
	c.Assert(int(TypeOf(v)), Equals, StringType)
	c.Assert(StringValue(v), Equals, "a string")
}

func (s *BuiltinsSuite) TestDefineNotSymbol(c *C) {
	code, _ := Parse(`(define "x" 5)`)
	_, err := Eval(code, Global)
	c.Assert(err, NotNil)
}

func (s *BuiltinsSuite) TestDefineFunction(c *C) {
	code, _ := Parse("(define (foo y) (+ y y))")
	_, err := Eval(code, Global)
	c.Assert(err, IsNil)

	code, err = Parse("(foo 5)")
	c.Assert(err, IsNil)
	v, err := Eval(code, Global)
	c.Assert(err, IsNil)

	c.Assert(v, NotNil)
	c.Assert(int(TypeOf(v)), Equals, IntegerType)
	c.Assert(IntegerValue(v), Equals, int64(10))
}

func (s *BuiltinsSuite) TestMapSingleCollection(c *C) {
	code, _ := Parse("(map (lambda (x) (* x 2)) (quote (1 2 3)))")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)

	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, ConsCellType)
	c.Assert(int(TypeOf(Car(result))), Equals, IntegerType)
	c.Assert(IntegerValue(Car(result)), Equals, int64(2))

	c.Assert(int(TypeOf(Cdr(result))), Equals, ConsCellType)
	c.Assert(int(TypeOf(Cadr(result))), Equals, IntegerType)
	c.Assert(IntegerValue(Cadr(result)), Equals, int64(4))

	c.Assert(int(TypeOf(Cddr(result))), Equals, ConsCellType)
	c.Assert(int(TypeOf(Third(result))), Equals, IntegerType)
	c.Assert(IntegerValue(Third(result)), Equals, int64(6))

	c.Assert(NilP(WalkList(result, "ddd")), Equals, true)
}

func (s *BuiltinsSuite) TestQuote(c *C) {
	code, _ := Parse("(quote (1 2))")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)

	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, ConsCellType)
	c.Assert(int(TypeOf(Car(result))), Equals, IntegerType)
	c.Assert(IntegerValue(Car(result)), Equals, int64(1))

	c.Assert(int(TypeOf(Cdr(result))), Equals, ConsCellType)
	c.Assert(int(TypeOf(Cadr(result))), Equals, IntegerType)
	c.Assert(IntegerValue(Cadr(result)), Equals, int64(2))

	c.Assert(NilP(Cddr(result)), Equals, true)
}

func (s *BuiltinsSuite) TestQuoteShortcut(c *C) {
	code, _ := Parse("'(1 2) ")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)

	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, ConsCellType)
	c.Assert(int(TypeOf(Car(result))), Equals, IntegerType)
	c.Assert(IntegerValue(Car(result)), Equals, int64(1))

	c.Assert(int(TypeOf(Cdr(result))), Equals, ConsCellType)
	c.Assert(int(TypeOf(Cadr(result))), Equals, IntegerType)
	c.Assert(IntegerValue(Cadr(result)), Equals, int64(2))

	c.Assert(NilP(Cddr(result)), Equals, true)
}
