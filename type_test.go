// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file tests the type functions.

package golisp

import (
	. "gopkg.in/check.v1"
)

type TypeSuite struct {
}

var _ = Suite(&TypeSuite{})

func (s *TypeSuite) TestList(c *C) {
	sexpr := Cons(IntegerWithValue(5), nil)
	c.Assert(ListP(sexpr), Equals, true)
	c.Assert(PairP(sexpr), Equals, true)
	c.Assert(int(TypeOf(sexpr)), Equals, ConsCellType)
	c.Assert(TypeName(ConsCellType), Equals, "List")
}

func (s *TypeSuite) TestInteger(c *C) {
	sexpr := IntegerWithValue(5)
	c.Assert(IntegerP(sexpr), Equals, true)
	c.Assert(int(TypeOf(sexpr)), Equals, IntegerType)
	c.Assert(TypeName(IntegerType), Equals, "Integer")
}

func (s *TypeSuite) TestFloat(c *C) {
	sexpr := FloatWithValue(5.0)
	c.Assert(FloatP(sexpr), Equals, true)
	c.Assert(int(TypeOf(sexpr)), Equals, FloatType)
	c.Assert(TypeName(FloatType), Equals, "Float")
}

func (s *TypeSuite) TestBoolean(c *C) {
	sexpr := BooleanWithValue(true)
	c.Assert(BooleanP(sexpr), Equals, true)
	c.Assert(int(TypeOf(sexpr)), Equals, BooleanType)
	c.Assert(TypeName(BooleanType), Equals, "Boolean")
}

func (s *TypeSuite) TestString(c *C) {
	sexpr := StringWithValue("str")
	c.Assert(StringP(sexpr), Equals, true)
	c.Assert(int(TypeOf(sexpr)), Equals, StringType)
	c.Assert(TypeName(StringType), Equals, "String")
}

func (s *TypeSuite) TestSymbol(c *C) {
	sexpr := SymbolWithName("str")
	c.Assert(SymbolP(sexpr), Equals, true)
	c.Assert(int(TypeOf(sexpr)), Equals, SymbolType)
	c.Assert(TypeName(SymbolType), Equals, "Symbol")
}

func (s *TypeSuite) TestFunction(c *C) {
	sexpr := FunctionWithNameParamsDocBodyAndParent("func", nil, nil, nil, nil)
	c.Assert(FunctionP(sexpr), Equals, true)
	c.Assert(int(TypeOf(sexpr)), Equals, FunctionType)
	c.Assert(TypeName(FunctionType), Equals, "Function")
}

func (s *TypeSuite) TestMacro(c *C) {
	sexpr := MacroWithNameParamsBodyAndParent("mac", nil, nil, nil)
	c.Assert(MacroP(sexpr), Equals, true)
	c.Assert(int(TypeOf(sexpr)), Equals, MacroType)
	c.Assert(TypeName(MacroType), Equals, "Macro")
}

func (s *TypeSuite) TestPrimitive(c *C) {
	sexpr := PrimitiveWithNameAndFunc("prim", nil)
	c.Assert(FunctionP(sexpr), Equals, false)
	c.Assert(PrimitiveP(sexpr), Equals, true)
	c.Assert(int(TypeOf(sexpr)), Equals, PrimitiveType)
	c.Assert(TypeName(PrimitiveType), Equals, "Primitive")
}

func (s *TypeSuite) TestObject(c *C) {
	sexpr := ObjectWithTypeAndValue("obj", nil)
	c.Assert(ObjectP(sexpr), Equals, true)
	c.Assert(int(TypeOf(sexpr)), Equals, BoxedObjectType)
	c.Assert(TypeName(BoxedObjectType), Equals, "Go Object")
}

func (s *TypeSuite) TestUnknown(c *C) {
	c.Assert(TypeName(255), Equals, "Unknown")
}
