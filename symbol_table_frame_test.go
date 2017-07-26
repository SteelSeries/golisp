// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file tests the symbol table frame.

package golisp

import (
	. "gopkg.in/check.v1"
)

type SymbolTableFrameSuite struct {
	frame *SymbolTableFrame
}

var _ = Suite(&SymbolTableFrameSuite{})

func (s *SymbolTableFrameSuite) SetUpTest(c *C) {
	s.frame = NewSymbolTableFrameBelow(nil, "test")
}

func (s *SymbolTableFrameSuite) TestFetching(c *C) {
	sym := SymbolWithName("test")
	b := BindingWithSymbolAndValue(sym, IntegerWithValue(42))
	s.frame.SetBindingAt("test", b)

	fetched, found := s.frame.BindingNamed("test")
	c.Assert(fetched, NotNil)
	c.Assert(found, Equals, true)

	c.Assert(StringValue(fetched.Sym), Equals, "test")
	c.Assert(IntegerValue(fetched.Val), Equals, int64(42))
}

func (s *SymbolTableFrameSuite) TestBinding(c *C) {
	_, err := s.frame.BindTo(Intern("test"), IntegerWithValue(42))
	c.Assert(err, IsNil)

	binding, found := s.frame.Bindings.Load("test")
	c.Assert(found, Equals, true)
	c.Assert(binding, NotNil)
}

func (s *SymbolTableFrameSuite) TestSymbolValue(c *C) {
	sym := Intern("test")
	_, err := s.frame.BindTo(sym, IntegerWithValue(42))
	c.Assert(err, IsNil)
	val := s.frame.ValueOf(sym)
	c.Assert(val, NotNil)
	c.Assert(int(TypeOf(val)), Equals, IntegerType)
	c.Assert(IntegerValue(val), Equals, int64(42))
}
