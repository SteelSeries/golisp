// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file tests the text output functionality.

package golisp

import (
    . "launchpad.net/gocheck"
)

type PrintingSuite struct{}

var _ = Suite(&PrintingSuite{})

func (s *PrintingSuite) TestNumber(c *C) {
    sexpr := NumberWithValue(5)
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
    sexpr := Cons(NumberWithValue(1), Cons(StringWithValue("two"), Cons(NumberWithValue(3), Cons(True, nil))))
    c.Assert(String(sexpr), Equals, `(1 "two" 3 #t)`)
}

func (s *PrintingSuite) TestNestedList(c *C) {
    sexpr := Cons(NumberWithValue(1), Cons(Cons(StringWithValue("two"), Cons(False, nil)), Cons(NumberWithValue(3), Cons(True, nil))))
    c.Assert(String(sexpr), Equals, `(1 ("two" #f) 3 #t)`)
}

func (s *PrintingSuite) TestDottedPair(c *C) {
    sexpr := Cons(NumberWithValue(1), StringWithValue("two"))
    c.Assert(String(sexpr), Equals, `(1 . "two")`)
}
