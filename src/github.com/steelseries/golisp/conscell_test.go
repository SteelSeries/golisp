// Copyright 2013 David R. Astels. All rights reserved.

//This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file inmpliments tests for cons cell
package golisp

import (
    . "launchpad.net/gocheck"

//    "testing"
)

type ConsCellSuite struct {
    a    *Data
    b    *Data
    cell *Data
}

var _ = Suite(&ConsCellSuite{})

func (s *ConsCellSuite) SetUpTest(c *C) {
    s.a = NumberWithValue(1)
    s.b = NumberWithValue(2)
    s.cell = Cons(s.a, s.b)
}

func (s *ConsCellSuite) TestCar(c *C) {
    c.Check(s.cell.Car, Equals, s.a)
}
