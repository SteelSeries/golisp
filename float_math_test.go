// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file tests built-in primitive functions.

package golisp

import (
	"fmt"
	"math"

	. "gopkg.in/check.v1"
)

type closeChecker struct {
	*CheckerInfo
}

// The Close checker verifies that the obtained value is within a tolerance of
// the expected value.
//
// For example:
//
//     c.Assert(value, Close, 42, 0.01)
//
var Close Checker = &closeChecker{
	&CheckerInfo{Name: "Close", Params: []string{"obtained", "expected", "tolerance"}},
}

func (checker *closeChecker) Check(params []interface{}, names []string) (result bool, error string) {
	defer func() {
		if v := recover(); v != nil {
			result = false
			error = fmt.Sprint(v)
		}
	}()
	return math.Abs(float64(params[0].(float64)-params[1].(float64))) < params[2].(float64), ""
}

type FloatBuiltinsSuite struct {
}

var _ = Suite(&FloatBuiltinsSuite{})

func (s *FloatBuiltinsSuite) SetUpSuite(c *C) {
	InitLisp()
}

func (s *FloatBuiltinsSuite) TestFloatAdd(c *C) {
	code, _ := Parse("(+ 1.2 2.3)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, FloatType)
	c.Assert(FloatValue(result), Close, float64(3.5), 0.01)
}

func (s *FloatBuiltinsSuite) TestFloatSubtract(c *C) {
	code, _ := Parse("(- 2.3 1.2)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, FloatType)
	c.Assert(FloatValue(result), Close, float64(1.1), 0.01)
}

func (s *FloatBuiltinsSuite) TestFloatSubtractWithNegativeResult(c *C) {
	code, _ := Parse("(- 1.2 2.3)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, FloatType)
	c.Assert(FloatValue(result), Close, float64(-1.1), 0.01)
}

func (s *FloatBuiltinsSuite) TestFloatMultiply(c *C) {
	code, _ := Parse("(* 2.3 1.2)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, FloatType)
	c.Assert(FloatValue(result), Close, float64(2.76), 0.01)
}

func (s *FloatBuiltinsSuite) TestFloatDivide(c *C) {
	code, _ := Parse("(/ 2.3 1.2)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(int(TypeOf(result)), Equals, FloatType)
	c.Assert(FloatValue(result), Close, float64(1.9167), 0.01)
}
