// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file tests the utilities.

package golisp

import (
    . "launchpad.net/gocheck"
)

type UtilitySuite struct {
}

var _ = Suite(&UtilitySuite{})

func (s *UtilitySuite) TestArrayToList(c *C) {
    a := []*Data{NumberWithValue(1), NumberWithValue(2)}
    sexpr := ArrayToList(a)
    c.Assert(ListP(sexpr), Equals, true)
    c.Assert(Length(sexpr), Equals, 2)
    c.Assert(NumericValue(Car(sexpr)), Equals, uint32(1))
    c.Assert(NumericValue(Cadr(sexpr)), Equals, uint32(2))
}

func (s *UtilitySuite) TestEmptyArrayToList(c *C) {
    a := []*Data{}
    sexpr := ArrayToList(a)
    c.Assert(ListP(sexpr), Equals, true)
    c.Assert(Length(sexpr), Equals, 0)
}

func (s *UtilitySuite) TestArrayToListWithTail(c *C) {
    a := []*Data{NumberWithValue(1), NumberWithValue(2)}
    tail := Cons(NumberWithValue(3), Cons(NumberWithValue(4), nil))
    sexpr := ArrayToListWithTail(a, tail)
    c.Assert(ListP(sexpr), Equals, true)
    c.Assert(Length(sexpr), Equals, 4)
    c.Assert(NumericValue(First(sexpr)), Equals, uint32(1))
    c.Assert(NumericValue(Second(sexpr)), Equals, uint32(2))
    c.Assert(NumericValue(Third(sexpr)), Equals, uint32(3))
    c.Assert(NumericValue(Fourth(sexpr)), Equals, uint32(4))
}

func (s *UtilitySuite) TestArrayToListWithEmptyTail(c *C) {
    a := []*Data{NumberWithValue(1), NumberWithValue(2)}
    sexpr := ArrayToListWithTail(a, nil)
    c.Assert(ListP(sexpr), Equals, true)
    c.Assert(Length(sexpr), Equals, 2)
    c.Assert(NumericValue(First(sexpr)), Equals, uint32(1))
    c.Assert(NumericValue(Second(sexpr)), Equals, uint32(2))
}

func (s *UtilitySuite) TestToArray(c *C) {
    list := Cons(NumberWithValue(1), Cons(NumberWithValue(2), nil))
    ary := ToArray(list)
    c.Assert(len(ary), Equals, 2)
    c.Assert(NumericValue(ary[0]), Equals, uint32(1))
    c.Assert(NumericValue(ary[1]), Equals, uint32(2))
}
