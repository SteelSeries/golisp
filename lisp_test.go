// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpreter for embedding in a go program for scripting.
// This file runs lisp based tests.

package golisp

import (
	. "gopkg.in/check.v1"
)

type LispSuite struct {
}

var _ = Suite(&LispSuite{})

func (s *LispSuite) TestLisp(c *C) {
	testCommand := "(run-all-tests \"tests\")"
	_, err := ProcessFile("lisp/testing.lsp")
	c.Assert(err, IsNil)
	data, err := ParseAndEval(testCommand)
	c.Assert(err, IsNil)
	c.Assert(BooleanP(data), Equals, true)
	c.Assert(BooleanValue(data), Equals, true)
}
