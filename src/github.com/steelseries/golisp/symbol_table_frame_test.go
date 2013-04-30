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
    s.frame = NewSymbolTableFrame()
}

func (s *SymbolTableFrameSuite) TestFetching(c *C) {
    sym := SymbolWithName("test")
    b := BindingWithSymbolAndValue(sym, NumberWithValue(42))
    s.frame.SetBindingAt("test", b)

    fetched, found := s.frame.BindingNamed("test")
    c.Assert(fetched, NotNil)
    c.Assert(found, Equals, true)

    c.Assert(StringValue(fetched.Sym), Equals, "test")
    c.Assert(IntValue(fetched.Val), Equals, 42)
}
