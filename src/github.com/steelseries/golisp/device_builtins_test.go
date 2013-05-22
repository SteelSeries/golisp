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

func GetField(source string) (field *DeviceField, err error) {
    code, err := Parse(source)
    if err != nil {
        return
    }
    fieldObj, err := Eval(code, Global)
    if err != nil {
        return
    }
    return (*DeviceField)(ObjectValue(fieldObj)), nil
}

func (s *DeviceBuiltinsSuite) TestBasicField(c *C) {
    field, err := GetField("(def-field test uint8)")
    c.Assert(err, IsNil)
    c.Assert(field, NotNil)
    c.Assert(field.Name, Equals, "test")
    c.Assert(field.TypeName, Equals, "uint8")
}
