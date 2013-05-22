// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file tests device description and serialization
package golisp

import (
    //"fmt"
    . "launchpad.net/gocheck"
)

type DeviceBuiltinsSuite struct {
}

var _ = Suite(&DeviceBuiltinsSuite{})

func (s *DeviceBuiltinsSuite) SetUpSuite(c *C) {
    Global = NewSymbolTableFrameBelow(nil)
    InitBuiltins()
    InitDeviceBuiltins()
}

func GetField(source string, c *C) (field *DeviceField) {
    code, err := Parse(source)
    c.Assert(err, IsNil)
    c.Assert(code, NotNil)
    fieldObj, err := Eval(code, Global)
    c.Assert(err, IsNil)
    c.Assert(fieldObj, NotNil)
    return (*DeviceField)(ObjectValue(fieldObj))
}

func (s *DeviceBuiltinsSuite) TestBasicField(c *C) {
    field := GetField("(def-field test uint8)", c)
    c.Assert(field.Name, Equals, "test")
    c.Assert(field.TypeName, Equals, "uint8")
}

func (s *DeviceBuiltinsSuite) TestFieldWithRepeat(c *C) {
    field := GetField("(def-field test uint8 (repeat 3))", c)
    c.Assert(field.RepeatCount, Equals, 3)
}

func (s *DeviceBuiltinsSuite) TestFieldWithRange(c *C) {
    field := GetField("(def-field test uint8 (range 3 5))", c)
    c.Assert(field.Validate(uint32(2)), Equals, false)
    c.Assert(field.Validate(uint32(3)), Equals, true)
    c.Assert(field.Validate(uint32(4)), Equals, true)
    c.Assert(field.Validate(uint32(5)), Equals, true)
    c.Assert(field.Validate(uint32(6)), Equals, false)
    c.Assert(field.Validate(uint32(42)), Equals, false)
}

func (s *DeviceBuiltinsSuite) TestFieldWithValues(c *C) {
    field := GetField("(def-field test uint8 (values 3 5 7 9))", c)
    c.Assert(field.Validate(uint32(2)), Equals, false)
    c.Assert(field.Validate(uint32(3)), Equals, true)
    c.Assert(field.Validate(uint32(4)), Equals, false)
    c.Assert(field.Validate(uint32(5)), Equals, true)
    c.Assert(field.Validate(uint32(6)), Equals, false)
    c.Assert(field.Validate(uint32(7)), Equals, true)
    c.Assert(field.Validate(uint32(8)), Equals, false)
    c.Assert(field.Validate(uint32(9)), Equals, true)
    c.Assert(field.Validate(uint32(10)), Equals, false)
    c.Assert(field.Validate(uint32(42)), Equals, false)
}

func (s *DeviceBuiltinsSuite) TestFieldWithListOfValues(c *C) {
    field := GetField("(def-field test uint8 (values '(3 5 7 9)))", c)
    c.Assert(field.Validate(uint32(2)), Equals, false)
    c.Assert(field.Validate(uint32(3)), Equals, true)
    c.Assert(field.Validate(uint32(4)), Equals, false)
    c.Assert(field.Validate(uint32(5)), Equals, true)
    c.Assert(field.Validate(uint32(6)), Equals, false)
    c.Assert(field.Validate(uint32(7)), Equals, true)
    c.Assert(field.Validate(uint32(8)), Equals, false)
    c.Assert(field.Validate(uint32(9)), Equals, true)
    c.Assert(field.Validate(uint32(10)), Equals, false)
    c.Assert(field.Validate(uint32(42)), Equals, false)
}

func (s *DeviceBuiltinsSuite) TestFieldReferencingPreviousField(c *C) {
    field := GetField("(def-field test uint8 (values '(3 5 7 9)))", c)
    c.Assert(field.Validate(uint32(2)), Equals, false)
    c.Assert(field.Validate(uint32(3)), Equals, true)
    c.Assert(field.Validate(uint32(4)), Equals, false)
    c.Assert(field.Validate(uint32(5)), Equals, true)
    c.Assert(field.Validate(uint32(6)), Equals, false)
    c.Assert(field.Validate(uint32(7)), Equals, true)
    c.Assert(field.Validate(uint32(8)), Equals, false)
    c.Assert(field.Validate(uint32(9)), Equals, true)
    c.Assert(field.Validate(uint32(10)), Equals, false)
    c.Assert(field.Validate(uint32(42)), Equals, false)
}
