// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpreter for embedding in a go program for scripting.
// This file tests the utilities.

package golisp

import (
	. "gopkg.in/check.v1"
)

type UtilitySuite struct {
}

var _ = Suite(&UtilitySuite{})

func (s *UtilitySuite) TestArrayToList(c *C) {
	a := []*Data{IntegerWithValue(1), IntegerWithValue(2)}
	sexpr := ArrayToList(a)
	c.Assert(ListP(sexpr), Equals, true)
	c.Assert(Length(sexpr), Equals, 2)
	c.Assert(IntegerValue(Car(sexpr)), Equals, int64(1))
	c.Assert(IntegerValue(Cadr(sexpr)), Equals, int64(2))
}

func (s *UtilitySuite) TestEmptyArrayToList(c *C) {
	a := []*Data{}
	sexpr := ArrayToList(a)
	c.Assert(ListP(sexpr), Equals, true)
	c.Assert(Length(sexpr), Equals, 0)
}

func (s *UtilitySuite) TestArrayToListWithTail(c *C) {
	a := []*Data{IntegerWithValue(1), IntegerWithValue(2)}
	tail := Cons(IntegerWithValue(3), Cons(IntegerWithValue(4), nil))
	sexpr := ArrayToListWithTail(a, tail)
	c.Assert(ListP(sexpr), Equals, true)
	c.Assert(Length(sexpr), Equals, 4)
	c.Assert(IntegerValue(First(sexpr)), Equals, int64(1))
	c.Assert(IntegerValue(Second(sexpr)), Equals, int64(2))
	c.Assert(IntegerValue(Third(sexpr)), Equals, int64(3))
	c.Assert(IntegerValue(Fourth(sexpr)), Equals, int64(4))
}

func (s *UtilitySuite) TestArrayToListWithEmptyTail(c *C) {
	a := []*Data{IntegerWithValue(1), IntegerWithValue(2)}
	sexpr := ArrayToListWithTail(a, nil)
	c.Assert(ListP(sexpr), Equals, true)
	c.Assert(Length(sexpr), Equals, 2)
	c.Assert(IntegerValue(First(sexpr)), Equals, int64(1))
	c.Assert(IntegerValue(Second(sexpr)), Equals, int64(2))
}

func (s *UtilitySuite) TestToArray(c *C) {
	list := Cons(IntegerWithValue(1), Cons(IntegerWithValue(2), nil))
	ary := ToArray(list)
	c.Assert(len(ary), Equals, 2)
	c.Assert(IntegerValue(ary[0]), Equals, int64(1))
	c.Assert(IntegerValue(ary[1]), Equals, int64(2))
}
