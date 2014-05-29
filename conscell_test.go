// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file impliments tests for cons cell.

package golisp

import (
	. "launchpad.net/gocheck"
)

type ConsCellSuite struct {
	a    *Data
	b    *Data
	cell *Data
}

var _ = Suite(&ConsCellSuite{})

func (s *ConsCellSuite) SetUpTest(c *C) {
	s.a = IntegerWithValue(1)
	s.b = IntegerWithValue(2)
	s.cell = Cons(s.a, s.b)
}

func (s *ConsCellSuite) TestCar(c *C) {
	c.Check(Car(s.cell), Equals, s.a)
}

func (s *ConsCellSuite) TestCarNil(c *C) {
	c.Check(Car(nil), IsNil)
}

func (s *ConsCellSuite) TestCdr(c *C) {
	c.Check(Cdr(s.cell), Equals, s.b)
}

func (s *ConsCellSuite) TestCdrNil(c *C) {
	c.Check(Cdr(nil), IsNil)
}
