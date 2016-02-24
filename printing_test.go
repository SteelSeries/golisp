// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file tests the text output functionality.

package golisp

import (
	"fmt"
	. "gopkg.in/check.v1"
	"unsafe"
)

type PrintingSuite struct{}

var _ = Suite(&PrintingSuite{})

func (s *PrintingSuite) TestInteger(c *C) {
	sexpr := IntegerWithValue(5)
	c.Assert(String(sexpr), Equals, "5")
}

func (s *PrintingSuite) TestTrue(c *C) {
	sexpr := BooleanWithValue(true)
	c.Assert(String(sexpr), Equals, "#t")
}

func (s *PrintingSuite) TestFalse(c *C) {
	sexpr := BooleanWithValue(false)
	c.Assert(String(sexpr), Equals, "#f")
}

func (s *PrintingSuite) TestString(c *C) {
	sexpr := StringWithValue("hello, world")
	c.Assert(String(sexpr), Equals, `"hello, world"`)
}

func (s *PrintingSuite) TestSymbol(c *C) {
	sexpr := SymbolWithName("function")
	c.Assert(String(sexpr), Equals, "function")
}

func (s *PrintingSuite) TestEmptyList(c *C) {
	sexpr := EmptyCons()
	c.Assert(String(sexpr), Equals, "()")
}

func (s *PrintingSuite) TestNil(c *C) {
	var sexpr *Data = nil
	c.Assert(String(sexpr), Equals, "()")
}

func (s *PrintingSuite) TestList(c *C) {
	sexpr := Cons(IntegerWithValue(1), Cons(StringWithValue("two"), Cons(IntegerWithValue(3), Cons(LispTrue, nil))))
	c.Assert(String(sexpr), Equals, `(1 "two" 3 #t)`)
}

func (s *PrintingSuite) TestNestedList(c *C) {
	sexpr := Cons(IntegerWithValue(1), Cons(Cons(StringWithValue("two"), Cons(LispFalse, nil)), Cons(IntegerWithValue(3), Cons(LispTrue, nil))))
	c.Assert(String(sexpr), Equals, `(1 ("two" #f) 3 #t)`)
}

func (s *PrintingSuite) TestDottedPair(c *C) {
	sexpr := Cons(IntegerWithValue(1), StringWithValue("two"))
	c.Assert(String(sexpr), Equals, `(1 . "two")`)
}

func (s *PrintingSuite) TestQuotedEmptyList(c *C) {
	sexpr := Cons(Intern("quote"), nil)
	c.Assert(String(sexpr), Equals, "'()")
}

func (s *PrintingSuite) TestAlist(c *C) {
	sexpr := Acons(IntegerWithValue(1), StringWithValue("two"), nil)
	c.Assert(String(sexpr), Equals, `((1 . "two"))`)
}

func (s *PrintingSuite) TestFunction(c *C) {
	sexpr := FunctionWithNameParamsDocBodyAndParent("func", nil, "", nil, nil)
	c.Assert(String(sexpr), Equals, "<function: func>")
}

func (s *PrintingSuite) TestMacro(c *C) {
	sexpr := MacroWithNameParamsBodyAndParent("mac", nil, nil, nil)
	c.Assert(String(sexpr), Equals, "<macro: mac>")
}

func (s *PrintingSuite) TestPrimitive(c *C) {
	f := &PrimitiveFunction{Name: "prim", NumberOfArgs: "1", Body: ListToBytesImpl}
	sexpr := PrimitiveWithNameAndFunc("prim", f)
	c.Assert(String(sexpr), Equals, "<prim: prim>")
}

func (s *PrintingSuite) TestObject(c *C) {
	sexpr := ObjectWithTypeAndValue("TypeSuite", unsafe.Pointer(s))
	c.Assert(String(sexpr), Equals, fmt.Sprintf("<opaque Go object of type TypeSuite : 0x%x>", (*uint64)(ObjectValue(sexpr))))
}

func (s *PrintingSuite) TestBytearray(c *C) {
	dataBytes := make([]byte, 5)
	for i := 0; i < 5; i++ {
		dataBytes[i] = byte(i + 1)
	}
	sexpr := ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&dataBytes))
	c.Assert(String(sexpr), Equals, "[1 2 3 4 5]")
}
