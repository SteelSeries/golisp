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

func (s *BuiltinsSuite) TestIfTrueWithThen(c *C) {
    code, _ := Parse("(if #t 5)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, NumberType)
    c.Assert(IntValue(result), Equals, 5)
}

func (s *BuiltinsSuite) TestIfFalseWithThen(c *C) {
    code, _ := Parse("(if #f 5)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, IsNil)
}

func (s *BuiltinsSuite) TestIfTrueWithThenAndElse(c *C) {
    code, _ := Parse("(if #t 5 10)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, NumberType)
    c.Assert(IntValue(result), Equals, 5)
}

func (s *BuiltinsSuite) TestIfFalseWithThenAndElse(c *C) {
    code, _ := Parse("(if #f 5 10)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, NumberType)
    c.Assert(IntValue(result), Equals, 10)
}

func (s *BuiltinsSuite) TestIfTrueWithNoClauses(c *C) {
    code, _ := Parse("(if #t)")
    result, err := Eval(code)
    c.Assert(err, NotNil)
    c.Assert(result, IsNil)
}

func (s *BuiltinsSuite) TestIfFalseWithNoClauses(c *C) {
    code, _ := Parse("(if #f)")
    result, err := Eval(code)
    c.Assert(err, NotNil)
    c.Assert(result, IsNil)
}

func (s *BuiltinsSuite) TestIfWithNoArgs(c *C) {
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

func (s *BuiltinsSuite) TestIfTrueWithMoreInvolvedArgs(c *C) {
    code, _ := Parse("(if #t (+ 3 2) (- 3 2))")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, NumberType)
    c.Assert(IntValue(result), Equals, 5)
}

func (s *BuiltinsSuite) TestIfFalseWithMoreInvolvedArgs(c *C) {
    code, _ := Parse("(if #f (+ 3 2) (- 3 2))")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, NumberType)
    c.Assert(IntValue(result), Equals, 1)
}

// %

func (s *BuiltinsSuite) TestRemainderWithNoArgs(c *C) {
    code, _ := Parse("(%)")
    _, err := Eval(code)
    c.Assert(err, NotNil)
}

func (s *BuiltinsSuite) TestRemainderWithOneArg(c *C) {
    code, _ := Parse("(% 5)")
    _, err := Eval(code)
    c.Assert(err, NotNil)
}

func (s *BuiltinsSuite) TestRemainder1(c *C) {
    code, _ := Parse("(% 5 2)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, NumberType)
    c.Assert(IntValue(result), Equals, 1)
}

func (s *BuiltinsSuite) TestRemainder3(c *C) {
    code, _ := Parse("(% 7 4)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, NumberType)
    c.Assert(IntValue(result), Equals, 3)
}

func (s *BuiltinsSuite) TestRemainder0(c *C) {
    code, _ := Parse("(% 7 7)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, NumberType)
    c.Assert(IntValue(result), Equals, 0)
}

// <

func (s *BuiltinsSuite) TestLessThanWithNoArgs(c *C) {
    code, _ := Parse("(<)")
    _, err := Eval(code)
    c.Assert(err, NotNil)
}

func (s *BuiltinsSuite) TestLessThanWithOneArg(c *C) {
    code, _ := Parse("(< 5)")
    _, err := Eval(code)
    c.Assert(err, NotNil)
}

func (s *BuiltinsSuite) TestLessThanFalse(c *C) {
    code, _ := Parse("(< 5 2)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, BooleanType)
    c.Assert(BooleanValue(result), Equals, false)
}

func (s *BuiltinsSuite) TestLessThanTrue(c *C) {
    code, _ := Parse("(< 2 5)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, BooleanType)
    c.Assert(BooleanValue(result), Equals, true)
}

func (s *BuiltinsSuite) TestLessThanBoundry(c *C) {
    code, _ := Parse("(< 2 2)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, BooleanType)
    c.Assert(BooleanValue(result), Equals, false)
}

// >

func (s *BuiltinsSuite) TestGreaterThanWithNoArgs(c *C) {
    code, _ := Parse("(>)")
    _, err := Eval(code)
    c.Assert(err, NotNil)
}

func (s *BuiltinsSuite) TestGreaterThanWithOneArg(c *C) {
    code, _ := Parse("(> 5)")
    _, err := Eval(code)
    c.Assert(err, NotNil)
}

func (s *BuiltinsSuite) TestGreaterThanFalse(c *C) {
    code, _ := Parse("(> 2 5)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, BooleanType)
    c.Assert(BooleanValue(result), Equals, false)
}

func (s *BuiltinsSuite) TestGreaterThanTrue(c *C) {
    code, _ := Parse("(> 5 2)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, BooleanType)
    c.Assert(BooleanValue(result), Equals, true)
}

func (s *BuiltinsSuite) TestGreaterThanBoundry(c *C) {
    code, _ := Parse("(> 2 2)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, BooleanType)
    c.Assert(BooleanValue(result), Equals, false)
}

// ==

func (s *BuiltinsSuite) TestEqualToWithNoArgs(c *C) {
    code, _ := Parse("(==)")
    _, err := Eval(code)
    c.Assert(err, NotNil)
}

func (s *BuiltinsSuite) TestEqualToWithOneArg(c *C) {
    code, _ := Parse("(== 5)")
    _, err := Eval(code)
    c.Assert(err, NotNil)
}

func (s *BuiltinsSuite) TestEqualToFalse(c *C) {
    code, _ := Parse("(== 2 5)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, BooleanType)
    c.Assert(BooleanValue(result), Equals, false)
}

func (s *BuiltinsSuite) TestEqualToTrue(c *C) {
    code, _ := Parse("(== 2 2)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, BooleanType)
    c.Assert(BooleanValue(result), Equals, true)
}

// !=

func (s *BuiltinsSuite) TestNotEqualToWithNoArgs(c *C) {
    code, _ := Parse("(!=)")
    _, err := Eval(code)
    c.Assert(err, NotNil)
}

func (s *BuiltinsSuite) TestNotEqualToWithOneArg(c *C) {
    code, _ := Parse("(!= 5)")
    _, err := Eval(code)
    c.Assert(err, NotNil)
}

func (s *BuiltinsSuite) TestNotEqualToFalse(c *C) {
    code, _ := Parse("(!= 2 2)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, BooleanType)
    c.Assert(BooleanValue(result), Equals, false)
}

func (s *BuiltinsSuite) TestNotEqualToTrue(c *C) {
    code, _ := Parse("(!= 2 5)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, BooleanType)
    c.Assert(BooleanValue(result), Equals, true)
}

// <=

func (s *BuiltinsSuite) TestLessThanEqualWithNoArgs(c *C) {
    code, _ := Parse("(<=)")
    _, err := Eval(code)
    c.Assert(err, NotNil)
}

func (s *BuiltinsSuite) TestLessThanEqualWithOneArg(c *C) {
    code, _ := Parse("(<= 5)")
    _, err := Eval(code)
    c.Assert(err, NotNil)
}

func (s *BuiltinsSuite) TestLessThanEqualFalse(c *C) {
    code, _ := Parse("(<= 5 2)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, BooleanType)
    c.Assert(BooleanValue(result), Equals, false)
}

func (s *BuiltinsSuite) TestLessThanEqualTrue(c *C) {
    code, _ := Parse("(<= 2 5)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, BooleanType)
    c.Assert(BooleanValue(result), Equals, true)
}

func (s *BuiltinsSuite) TestLessThanEqualBoundry(c *C) {
    code, _ := Parse("(<= 2 2)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, BooleanType)
    c.Assert(BooleanValue(result), Equals, true)
}

// >=

func (s *BuiltinsSuite) TestGreaterThanEqualWithNoArgs(c *C) {
    code, _ := Parse("(>=)")
    _, err := Eval(code)
    c.Assert(err, NotNil)
}

func (s *BuiltinsSuite) TestGreaterThanEqualWithOneArg(c *C) {
    code, _ := Parse("(>= 5)")
    _, err := Eval(code)
    c.Assert(err, NotNil)
}

func (s *BuiltinsSuite) TestGreaterThanEqualFalse(c *C) {
    code, _ := Parse("(>= 2 5)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, BooleanType)
    c.Assert(BooleanValue(result), Equals, false)
}

func (s *BuiltinsSuite) TestGreaterThanEqualTrue(c *C) {
    code, _ := Parse("(>= 5 2)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, BooleanType)
    c.Assert(BooleanValue(result), Equals, true)
}

func (s *BuiltinsSuite) TestGreaterThanEqualBoundry(c *C) {
    code, _ := Parse("(>= 2 2)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, BooleanType)
    c.Assert(BooleanValue(result), Equals, true)
}

// !

func (s *BuiltinsSuite) TestNotWithNoArgs(c *C) {
    code, _ := Parse("(!)")
    _, err := Eval(code)
    c.Assert(err, NotNil)
}

func (s *BuiltinsSuite) TestNotWithFalse(c *C) {
    code, _ := Parse("(! #f)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, BooleanType)
    c.Assert(BooleanValue(result), Equals, true)
}

func (s *BuiltinsSuite) TestNotWithTrue(c *C) {
    code, _ := Parse("(! #t)")
    result, err := Eval(code)
    c.Assert(err, IsNil)
    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, BooleanType)
    c.Assert(BooleanValue(result), Equals, false)
}

func (s *BuiltinsSuite) TestDefine(c *C) {
    code, _ := Parse("(define x 5)")
    Eval(code)
    sym := Intern("x")
    v := ValueOf(sym)
    c.Assert(v, NotNil)
    c.Assert(TypeOf(v), Equals, NumberType)
    c.Assert(IntValue(v), Equals, 5)
}

func (s *BuiltinsSuite) TestDefineNotSymbol(c *C) {
    code, _ := Parse(`(define "x" 5)`)
    _, err := Eval(code)
    c.Assert(err, NotNil)
}

func (s *BuiltinsSuite) TestDefineFunction(c *C) {
    code, _ := Parse("(define (foo y) (+ y y))")
    _, err := Eval(code)
    c.Assert(err, IsNil)

    code, err = Parse("(foo 5)")
    c.Assert(err, IsNil)
    v, err := Eval(code)
    c.Assert(err, IsNil)

    c.Assert(v, NotNil)
    c.Assert(TypeOf(v), Equals, NumberType)
    c.Assert(IntValue(v), Equals, 10)
}

func (s *BuiltinsSuite) TestMapSingleCollection(c *C) {
    code, _ := Parse("(map (lambda (x) (* x 2)) (quote (1 2 3)))")
    result, err := Eval(code)
    c.Assert(err, IsNil)

    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, ConsCellType)
    c.Assert(TypeOf(Car(result)), Equals, NumberType)
    c.Assert(IntValue(Car(result)), Equals, 2)

    c.Assert(TypeOf(Cdr(result)), Equals, ConsCellType)
    c.Assert(TypeOf(Cadr(result)), Equals, NumberType)
    c.Assert(IntValue(Cadr(result)), Equals, 4)

    c.Assert(TypeOf(Cddr(result)), Equals, ConsCellType)
    c.Assert(TypeOf(Caddr(result)), Equals, NumberType)
    c.Assert(IntValue(Caddr(result)), Equals, 6)

    c.Assert(NilP(Cdddr(result)), Equals, true)
}

// func (s *BuiltinsSuite) TestMapMultipleCollection(c *C) {
//     code, _ := Parse("(map (lambda (x y) (* x y)) (quote (1 2 3)) (quote (4 5 6)))")
//     result, err := Eval(code)
//     c.Assert(err, IsNil)

//     c.Assert(result, NotNil)
//     c.Assert(TypeOf(result), Equals, ConsCellType)
//     c.Assert(TypeOf(Car(result)), Equals, NumberType)
//     c.Assert(IntValue(Car(result)), Equals, 5)

//     c.Assert(TypeOf(Cdr(result)), Equals, ConsCellType)
//     c.Assert(TypeOf(Cadr(result)), Equals, NumberType)
//     c.Assert(IntValue(Cadr(result)), Equals, 7)

//     c.Assert(TypeOf(Cddr(result)), Equals, ConsCellType)
//     c.Assert(TypeOf(Caddr(result)), Equals, NumberType)
//     c.Assert(IntValue(Caddr(result)), Equals, 9)

//     c.Assert(NilP(Cdddr(result)), Equals, true)
// }

func (s *BuiltinsSuite) TestQuote(c *C) {
    code, _ := Parse("(quote (1 2))")
    result, err := Eval(code)
    c.Assert(err, IsNil)

    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, ConsCellType)
    c.Assert(TypeOf(Car(result)), Equals, NumberType)
    c.Assert(IntValue(Car(result)), Equals, 1)

    c.Assert(TypeOf(Cdr(result)), Equals, ConsCellType)
    c.Assert(TypeOf(Cadr(result)), Equals, NumberType)
    c.Assert(IntValue(Cadr(result)), Equals, 2)

    c.Assert(NilP(Cddr(result)), Equals, true)
}

func (s *BuiltinsSuite) TestQuoteShortcut(c *C) {
    code, _ := Parse("'(1 2)")
    result, err := Eval(code)
    c.Assert(err, IsNil)

    c.Assert(result, NotNil)
    c.Assert(TypeOf(result), Equals, ConsCellType)
    c.Assert(TypeOf(Car(result)), Equals, NumberType)
    c.Assert(IntValue(Car(result)), Equals, 1)

    c.Assert(TypeOf(Cdr(result)), Equals, ConsCellType)
    c.Assert(TypeOf(Cadr(result)), Equals, NumberType)
    c.Assert(IntValue(Cadr(result)), Equals, 2)

    c.Assert(NilP(Cddr(result)), Equals, true)
}
