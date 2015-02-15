// Copyright 2015 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file tests the bytecode runtime.

package golisp

import (
	. "launchpad.net/gocheck"
)

type BytecodeSuite struct {
}

var _ = Suite(&BytecodeSuite{})

func (s *BytecodeSuite) SetUpSuite(c *C) {
	Global = NewSymbolTableFrameBelow(nil)
	InitBuiltins()
}

func (s *BytecodeSuite) TestShortConstant(c *C) {
	code := []uint16{0x1042}
	_, _ = ExecuteBytecode(code, Global)
	c.Assert(BytecodeStackPointer, Equals, 1)
	data := BytecodeDataStack[0]
	c.Assert(int(TypeOf(data)), Equals, IntegerType)
	c.Assert(IntegerValue(data), Equals, int64(0x42))
}
