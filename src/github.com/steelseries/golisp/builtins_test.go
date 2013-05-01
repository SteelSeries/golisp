// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file tests builtin primitive functions
package golisp

import (
    "container/list"
    . "launchpad.net/gocheck"
)

type BuiltinsSuite struct {
}

var _ = Suite(&BuiltinsSuite{})

func (s *BuiltinsSuite) SetUpSuite(c *C) {
    symbolTable = &SymbolTable{list.New()}
    PushLocalBindings()
    InitBuiltins()
    //    symbolTable.Dump()
}

// Add

func (s *BuiltinsSuite) TestUnaryAdd(c *C) {
    code, _ := Parse("(+ 1)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, NumberType)
    c.Assert(IntValue(result), Equals, 1)
}

func (s *BuiltinsSuite) TestBinaryAdd(c *C) {
    code, _ := Parse("(+ 1 2)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, NumberType)
    c.Assert(IntValue(result), Equals, 3)
}

func (s *BuiltinsSuite) TestTrinaryAdd(c *C) {
    code, _ := Parse("(+ 1 2 3)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, NumberType)
    c.Assert(IntValue(result), Equals, 6)
}

// Subtract

func (s *BuiltinsSuite) TestNegate(c *C) {
    code, _ := Parse("(- 1)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, NumberType)
    c.Assert(IntValue(result), Equals, -1)
}

func (s *BuiltinsSuite) TestBinarySubtract(c *C) {
    code, _ := Parse("(- 2 1)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, NumberType)
    c.Assert(IntValue(result), Equals, 1)
}

func (s *BuiltinsSuite) TestTrinarySubtract(c *C) {
    code, _ := Parse("(- 3 2 1)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, NumberType)
    c.Assert(IntValue(result), Equals, 0)
}

func (s *BuiltinsSuite) TestNegativeResult(c *C) {
    code, _ := Parse("(- 2 4)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, NumberType)
    c.Assert(IntValue(result), Equals, -2)
}

// Mutiply

func (s *BuiltinsSuite) TestUnaryMultiply(c *C) {
    code, _ := Parse("(* 2)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, NumberType)
    c.Assert(IntValue(result), Equals, 2)
}

func (s *BuiltinsSuite) TestBinaryMultiplty(c *C) {
    code, _ := Parse("(* 2 3)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, NumberType)
    c.Assert(IntValue(result), Equals, 6)
}

func (s *BuiltinsSuite) TestTrinaryMultiply(c *C) {
    code, _ := Parse("(* 2 3 4)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, NumberType)
    c.Assert(IntValue(result), Equals, 24)
}

// Quotient

func (s *BuiltinsSuite) TestUnaryQuotient(c *C) {
    code, _ := Parse("(/ 24)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, NumberType)
    c.Assert(IntValue(result), Equals, 24)
}

func (s *BuiltinsSuite) TestBinaryQuotient(c *C) {
    code, _ := Parse("(/ 24 2)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, NumberType)
    c.Assert(IntValue(result), Equals, 12)
}

func (s *BuiltinsSuite) TestTrinaryQuotient(c *C) {
    code, _ := Parse("(/ 24 2 3)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, NumberType)
    c.Assert(IntValue(result), Equals, 4)
}

// compound

func (s *BuiltinsSuite) TestCompoundMath(c *C) {
    code, _ := Parse("(+ 1 (* 2 3))")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, NumberType)
    c.Assert(IntValue(result), Equals, 7)
}

// If

func (s *BuiltinsSuite) TestTrueWithThen(c *C) {
    code, _ := Parse("(if #t 5)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, NumberType)
    c.Assert(IntValue(result), Equals, 5)
}

func (s *BuiltinsSuite) TestFalseWithThen(c *C) {
    code, _ := Parse("(if #f 5)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, IsNil)
}

func (s *BuiltinsSuite) TestTrueWithThenAndElse(c *C) {
    code, _ := Parse("(if #t 5 10)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, NumberType)
    c.Assert(IntValue(result), Equals, 5)
}

func (s *BuiltinsSuite) TestFalseWithThenAndElse(c *C) {
    code, _ := Parse("(if #f 5 10)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, NumberType)
    c.Assert(IntValue(result), Equals, 10)
}

func (s *BuiltinsSuite) TestTrueWithNoClauses(c *C) {
    code, _ := Parse("(if #t)")
    result, err := Eval(code)
    c.Assert(err, NotNil)
    c.Assert(result, IsNil)
}

func (s *BuiltinsSuite) TestFalseWithNoClauses(c *C) {
    code, _ := Parse("(if #f)")
    result, err := Eval(code)
    c.Assert(err, NotNil)
    c.Assert(result, IsNil)
}

func (s *BuiltinsSuite) TestWithNoArgs(c *C) {
    code, _ := Parse("(if)")
    result, err := Eval(code)
    c.Assert(err, NotNil)
    c.Assert(result, IsNil)
}

func (s *BuiltinsSuite) TestWithTooManyArgs(c *C) {
    code, _ := Parse("(if #f 2 3 4)")
    result, err := Eval(code)
    c.Assert(err, NotNil)
    c.Assert(result, IsNil)
}

func (s *BuiltinsSuite) TestTrueWithMoreInvolvedArgs(c *C) {
    code, _ := Parse("(if #t (+ 3 2) (- 3 2))")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, NumberType)
    c.Assert(IntValue(result), Equals, 5)
}

func (s *BuiltinsSuite) TestFalseWithMoreInvolvedArgs(c *C) {
    code, _ := Parse("(if #f (+ 3 2) (- 3 2))")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, NumberType)
    c.Assert(IntValue(result), Equals, 1)
}
