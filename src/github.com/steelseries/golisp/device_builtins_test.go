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

func GetField(source string, c *C) (f *DeviceField, field *ExpandedField) {
    code, err := Parse(source)
    c.Assert(err, IsNil)
    c.Assert(code, NotNil)
    fieldObj, err := Eval(code, Global)
    c.Assert(err, IsNil)
    c.Assert(fieldObj, NotNil)
    f = (*DeviceField)(ObjectValue(fieldObj))
    return f, &ExpandedField{FieldDefinition: f, Offset: 0, Size: f.Size, Path: ""}
}

func (s *DeviceBuiltinsSuite) TestBasicField(c *C) {
    f, _ := GetField("(def-field test uint8)", c)
    c.Assert(f.Name, Equals, "test")
    c.Assert(f.TypeName, Equals, "uint8")
}

func (s *DeviceBuiltinsSuite) TestFieldWithRepeat(c *C) {
    f, _ := GetField("(def-field test uint8 (repeat 3))", c)
    c.Assert(f.RepeatCount, Equals, 3)
}

func (s *DeviceBuiltinsSuite) TestFieldWithRange(c *C) {
    _, field := GetField("(def-field test uint8 (range 3 5))", c)
    field.Value = uint32(2)
    c.Assert(field.Validate(Global), Equals, false)
    field.Value = uint32(3)
    c.Assert(field.Validate(Global), Equals, true)
    field.Value = uint32(4)
    c.Assert(field.Validate(Global), Equals, true)
    field.Value = uint32(5)
    c.Assert(field.Validate(Global), Equals, true)
    field.Value = uint32(6)
    c.Assert(field.Validate(Global), Equals, false)
    field.Value = uint32(42)
    c.Assert(field.Validate(Global), Equals, false)
}

func (s *DeviceBuiltinsSuite) TestFieldWithValues(c *C) {
    _, field := GetField("(def-field test uint8 (values 3 5 7 9))", c)
    field.Value = uint32(2)
    c.Assert(field.Validate(Global), Equals, false)
    field.Value = uint32(3)
    c.Assert(field.Validate(Global), Equals, true)
    field.Value = uint32(4)
    c.Assert(field.Validate(Global), Equals, false)
    field.Value = uint32(5)
    c.Assert(field.Validate(Global), Equals, true)
    field.Value = uint32(6)
    c.Assert(field.Validate(Global), Equals, false)
    field.Value = uint32(7)
    c.Assert(field.Validate(Global), Equals, true)
    field.Value = uint32(8)
    c.Assert(field.Validate(Global), Equals, false)
    field.Value = uint32(9)
    c.Assert(field.Validate(Global), Equals, true)
    field.Value = uint32(10)
    c.Assert(field.Validate(Global), Equals, false)
    field.Value = uint32(42)
    c.Assert(field.Validate(Global), Equals, false)
}

func (s *DeviceBuiltinsSuite) TestFieldWithListOfValues(c *C) {
    _, field := GetField("(def-field test uint8 (values '(3 5 7 9)))", c)
    field.Value = uint32(2)
    c.Assert(field.Validate(Global), Equals, false)
    field.Value = uint32(3)
    c.Assert(field.Validate(Global), Equals, true)
    field.Value = uint32(4)
    c.Assert(field.Validate(Global), Equals, false)
    field.Value = uint32(5)
    c.Assert(field.Validate(Global), Equals, true)
    field.Value = uint32(6)
    c.Assert(field.Validate(Global), Equals, false)
    field.Value = uint32(7)
    c.Assert(field.Validate(Global), Equals, true)
    field.Value = uint32(8)
    c.Assert(field.Validate(Global), Equals, false)
    field.Value = uint32(9)
    c.Assert(field.Validate(Global), Equals, true)
    field.Value = uint32(10)
    c.Assert(field.Validate(Global), Equals, false)
    field.Value = uint32(42)
    c.Assert(field.Validate(Global), Equals, false)
}

func (s *DeviceBuiltinsSuite) TestFieldReferencingPreviousField(c *C) {
    _, field := GetField("(def-field test uint8 (values '(3 5 7 9)))", c)
    field.Value = uint32(2)
    c.Assert(field.Validate(Global), Equals, false)
    field.Value = uint32(3)
    c.Assert(field.Validate(Global), Equals, true)
    field.Value = uint32(4)
    c.Assert(field.Validate(Global), Equals, false)
    field.Value = uint32(5)
    c.Assert(field.Validate(Global), Equals, true)
    field.Value = uint32(6)
    c.Assert(field.Validate(Global), Equals, false)
    field.Value = uint32(7)
    c.Assert(field.Validate(Global), Equals, true)
    field.Value = uint32(8)
    c.Assert(field.Validate(Global), Equals, false)
    field.Value = uint32(9)
    c.Assert(field.Validate(Global), Equals, true)
    field.Value = uint32(10)
    c.Assert(field.Validate(Global), Equals, false)
    field.Value = uint32(42)
    c.Assert(field.Validate(Global), Equals, false)
}

func (s *DeviceBuiltinsSuite) TestFieldWithDeferredValues(c *C) {
    _, field := GetField("(def-field test uint8 (deferred-validation (values 3)))", c)

    field.Value = uint32(2)
    c.Assert(field.Validate(Global), Equals, false)
    field.Value = uint32(3)
    c.Assert(field.Validate(Global), Equals, true)
    field.Value = uint32(4)
    c.Assert(field.Validate(Global), Equals, false)
}

func (s *DeviceBuiltinsSuite) TestFieldWithDeferredValuesReferencingPreviousFields(c *C) {
    source := "(def-struct test-struct" +
        "(def-field f1 uint8)" +
        "(def-field f2 uint8 (deferred-validation (case f1 (0 (values 1)) (1 (range 10 11))))))"

    code, err := Parse(source)
    c.Assert(err, IsNil)
    c.Assert(code, NotNil)
    _, err = Eval(code, Global)
    c.Assert(err, IsNil)

    structObj := Global.ValueOf(SymbolWithName("test-struct"))
    ds := (*DeviceStructure)(ObjectValue(structObj))
    es := ds.Expand()
    f1 := es.Fields[0]
    f2 := es.Fields[1]

    f1.Value = uint32(0)
    f2.Value = uint32(0)
    c.Assert(es.Validate(), Equals, false)
    f2.Value = uint32(1)
    c.Assert(es.Validate(), Equals, true)
    f2.Value = uint32(2)
    c.Assert(es.Validate(), Equals, false)

    f1.Value = uint32(1)
    f2.Value = uint32(9)
    c.Assert(es.Validate(), Equals, false)
    f2.Value = uint32(10)
    c.Assert(es.Validate(), Equals, true)
    f2.Value = uint32(11)
    c.Assert(es.Validate(), Equals, true)
    f2.Value = uint32(12)
    c.Assert(es.Validate(), Equals, false)

}
