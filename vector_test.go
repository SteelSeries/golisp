// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file tests the integer type.

package golisp

import (
	. "gopkg.in/check.v1"
)

type VectorSuite struct {
	ary []*Data
}

var _ = Suite(&VectorSuite{})

func (s *VectorSuite) SetUpTest(c *C) {
	s.ary = make([]*Data, 5)
	s.ary[0] = IntegerWithValue(1)
	s.ary[1] = IntegerWithValue(2)
	s.ary[2] = IntegerWithValue(3)
	s.ary[3] = IntegerWithValue(4)
	s.ary[4] = IntegerWithValue(5)
}

func (s *VectorSuite) TestVectorValue(c *C) {
	v := VectorValue(VectorWithValue(s.ary))
	c.Assert(int(IntegerValue(v[0])), Equals, 1)
	c.Assert(int(IntegerValue(v[1])), Equals, 2)
	c.Assert(int(IntegerValue(v[2])), Equals, 3)
	c.Assert(int(IntegerValue(v[3])), Equals, 4)
	c.Assert(int(IntegerValue(v[4])), Equals, 5)
}

func (s *VectorSuite) TestVectorValues(c *C) {
	v := VectorValue(VectorWithValues(IntegerWithValue(1),
		IntegerWithValue(2),
		IntegerWithValue(3),
		IntegerWithValue(4),
		IntegerWithValue(5)))
	c.Assert(int(IntegerValue(v[0])), Equals, 1)
	c.Assert(int(IntegerValue(v[1])), Equals, 2)
	c.Assert(int(IntegerValue(v[2])), Equals, 3)
	c.Assert(int(IntegerValue(v[3])), Equals, 4)
	c.Assert(int(IntegerValue(v[4])), Equals, 5)
}

func (s *VectorSuite) TestVectorTypeCheck(c *C) {
	v := VectorWithValue(s.ary)
	c.Assert(VectorP(v), Equals, true)
}
