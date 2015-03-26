// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file tests function evaluation.

package golisp

import (
	. "launchpad.net/gocheck"
)

type EvalSuite struct {
}

var _ = Suite(&EvalSuite{})

func (s *EvalSuite) SetUpSuite(c *C) {
	Global = NewSymbolTableFrameBelow(nil, "SystemGlobal")
	InitBuiltins()
}

func (s *EvalSuite) TestEvalWithNilFunction(c *C) {
	code, _ := Parse("(no-function-named-this)")
	result, err := Eval(code, Global)
	c.Assert(err, NotNil)
	c.Assert(result, IsNil)
}
