// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file tests the symbol table
package golisp

import (
    "container/list"
    . "launchpad.net/gocheck"
)

type SymbolTableSuite struct {
}

var _ = Suite(&SymbolTableSuite{})

func (s *SymbolTableSuite) SetUpTest(c *C) {
    symbolTable = &SymbolTable{list.New()}
    PushLocalBindings()
}

func (s *SymbolTableSuite) TestInterning(c *C) {
    Intern("test")
    sym, found := findSymbol("test")
    c.Assert(found, Equals, true)
    c.Assert(TypeOf(sym), Equals, SymbolType)
    c.Assert(StringValue(sym), Equals, "test")
}

func (s *SymbolTableSuite) TestBinding(c *C) {
    sym := Intern("test")
    BindTo(sym, NumberWithValue(42))

    frame := symbolTable.Frames.Front().Value.(*SymbolTableFrame)
    binding, found := frame.Bindings["test"]
    c.Assert(found, Equals, true)
    c.Assert(binding, NotNil)
}

func (s *SymbolTableSuite) TestSymbolValue(c *C) {
    sym := Intern("test")
    BindTo(sym, NumberWithValue(42))
    val := ValueOf(sym)
    c.Assert(val, NotNil)
    c.Assert(TypeOf(val), Equals, NumberType)
    c.Assert(IntValue(val), Equals, 42)
}
