// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file tests the symbol table frame
package golisp

import (
    . "launchpad.net/gocheck"
)

type SymbolTableFrameSuite struct {
    frame *SymbolTableFrame
}

var _ = Suite(&SymbolTableFrameSuite{})

func (s *SymbolTableFrameSuite) SetUpTest(c *C) {
    s.frame = NewSymbolTableFrameBelow(nil)
}

func (s *SymbolTableFrameSuite) TestFetching(c *C) {
    sym := SymbolWithName("test")
    b := BindingWithSymbolAndValue(sym, NumberWithValue(42))
    s.frame.SetBindingAt("test", b)

    fetched, found := s.frame.BindingNamed("test")
    c.Assert(fetched, NotNil)
    c.Assert(found, Equals, true)

    c.Assert(StringValue(fetched.Sym), Equals, "test")
    c.Assert(NumericValue(fetched.Val), Equals, uint32(42))
}

func (s *SymbolTableFrameSuite) TestInterning(c *C) {
    s.frame.Intern("test")
    sym, found := s.frame.findSymbol("test")
    c.Assert(found, Equals, true)
    c.Assert(TypeOf(sym), Equals, SymbolType)
    c.Assert(StringValue(sym), Equals, "test")
}

func (s *SymbolTableFrameSuite) TestBinding(c *C) {
    sym := s.frame.Intern("test")
    s.frame.BindTo(sym, NumberWithValue(42))

    binding, found := s.frame.Bindings["test"]
    c.Assert(found, Equals, true)
    c.Assert(binding, NotNil)
}

func (s *SymbolTableFrameSuite) TestSymbolValue(c *C) {
    sym := s.frame.Intern("test")
    s.frame.BindTo(sym, NumberWithValue(42))
    val := s.frame.ValueOf(sym)
    c.Assert(val, NotNil)
    c.Assert(TypeOf(val), Equals, NumberType)
    c.Assert(NumericValue(val), Equals, uint32(42))
}
