// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file tests the Object data type
package golisp

import (
    . "launchpad.net/gocheck"
    "unsafe"
)

type ObjectAtomSuite struct {
    o *Data
}

var _ = Suite(&ObjectAtomSuite{})

type TestStruct struct {
    D int
}

func (s *ObjectAtomSuite) SetUpTest(c *C) {
    obj := &TestStruct{D: 5}
    s.o = ObjectWithValue(unsafe.Pointer(obj))
    c.Assert((*map[string]int)(ObjectValue(s.o)), Equals, obj)
}
