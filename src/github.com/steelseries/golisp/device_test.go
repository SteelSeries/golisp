// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file tests device description and serialization
package golisp

import (
    . "launchpad.net/gocheck"
    "unsafe"
)

type DeviceSuite struct {
}

var _ = Suite(&DeviceSuite{})

func (s *DeviceSuite) TestSingleUint8(c *C) {
    st := NewStruct("test")
    df := NewField("f1", "uint8", 1, 1, nil, nil)
    st.AddField(df)
    exp := st.Expand()
    f := exp.Fields[0]
    c.Assert(f.FieldDefinition, Equals, df)
    c.Assert(f.Offset, Equals, 0)
    c.Assert(f.Size, Equals, 1)
    c.Assert(exp.Size, Equals, 1)
}

func (s *DeviceSuite) TestUint8AndUint8(c *C) {
    st := NewStruct("test")
    st.AddField(NewField("f1", "uint8", 1, 1, nil, nil))
    st.AddField(NewField("f2", "uint8", 1, 1, nil, nil))
    exp := st.Expand()
    f1 := exp.Fields[0]
    c.Assert(f1.Offset, Equals, 0)
    c.Assert(f1.Size, Equals, 1)
    f2 := exp.Fields[1]
    c.Assert(f2.Offset, Equals, 1)
    c.Assert(f2.Size, Equals, 1)
    c.Assert(exp.Size, Equals, 2)
}

func (s *DeviceSuite) TestUint8AndUint16(c *C) {
    st := NewStruct("test")
    st.AddField(NewField("f1", "uint8", 1, 1, nil, nil))
    st.AddField(NewField("f2", "uint16", 2, 1, nil, nil))
    exp := st.Expand()
    f1 := exp.Fields[0]
    c.Assert(f1.Offset, Equals, 0)
    c.Assert(f1.Size, Equals, 1)
    f2 := exp.Fields[1]
    c.Assert(f2.Offset, Equals, 2)
    c.Assert(f2.Size, Equals, 2)
    c.Assert(exp.Size, Equals, 4)
}

func (s *DeviceSuite) TestUint8AndUint32(c *C) {
    st := NewStruct("test")
    st.AddField(NewField("f1", "uint8", 1, 1, nil, nil))
    st.AddField(NewField("f2", "uint32", 4, 1, nil, nil))
    exp := st.Expand()
    f1 := exp.Fields[0]
    c.Assert(f1.Offset, Equals, 0)
    c.Assert(f1.Size, Equals, 1)
    f2 := exp.Fields[1]
    c.Assert(f2.Offset, Equals, 4)
    c.Assert(f2.Size, Equals, 4)
    c.Assert(exp.Size, Equals, 8)
}

func (s *DeviceSuite) TestSingleUint8Bytes(c *C) {
    st := NewStruct("test")
    st.AddField(NewField("f1", "uint8", 1, 1, nil, nil))
    exp := st.Expand()
    f := exp.Fields[0]
    f.Value = 5
    bytes := *exp.ByteArray()
    c.Assert(bytes[0], Equals, byte(5))
}

func (s *DeviceSuite) TestUint8AndUint8Bytes(c *C) {
    st := NewStruct("test")
    st.AddField(NewField("f1", "uint8", 1, 1, nil, nil))
    st.AddField(NewField("f2", "uint8", 1, 1, nil, nil))
    exp := st.Expand()
    exp.Fields[0].Value = 47
    exp.Fields[1].Value = 185
    bytes := *exp.ByteArray()
    c.Assert(bytes[0], Equals, byte(47))
    c.Assert(bytes[1], Equals, byte(185))
}

func (s *DeviceSuite) TestUint8AndUint16Bytes(c *C) {
    st := NewStruct("test")
    st.AddField(NewField("f1", "uint8", 1, 1, nil, nil))
    st.AddField(NewField("f2", "uint16", 2, 1, nil, nil))
    exp := st.Expand()
    exp.Fields[0].Value = 47
    exp.Fields[1].Value = 22970
    bytes := *exp.ByteArray()
    c.Assert(bytes[0], Equals, byte(47))
    c.Assert(bytes[1], Equals, byte(0))
    c.Assert(bytes[2], Equals, byte(0xBA))
    c.Assert(bytes[3], Equals, byte(0x59))
}

func (s *DeviceSuite) TestUint8AndUint32Bytes(c *C) {
    st := NewStruct("test")
    st.AddField(NewField("f1", "uint8", 1, 1, nil, nil))
    st.AddField(NewField("f2", "uint32", 4, 1, nil, nil))
    exp := st.Expand()
    exp.Fields[0].Value = 47
    exp.Fields[1].Value = 702831034
    bytes := *exp.ByteArray()
    c.Assert(bytes[0], Equals, byte(47))
    c.Assert(bytes[1], Equals, byte(0))
    c.Assert(bytes[2], Equals, byte(0))
    c.Assert(bytes[3], Equals, byte(0))
    c.Assert(bytes[4], Equals, byte(0xBA))
    c.Assert(bytes[5], Equals, byte(0x59))
    c.Assert(bytes[6], Equals, byte(0xE4))
    c.Assert(bytes[7], Equals, byte(0x29))
}

func (s *DeviceSuite) TestUint8AndUint8FromJson(c *C) {
    st := NewStruct("test")
    st.AddField(NewField("f1", "uint8", 1, 1, nil, nil))
    st.AddField(NewField("f2", "uint8", 1, 1, nil, nil))
    exp := st.Expand()

    json := `{"f1": 47, "f2": 185}`

    exp.PopulateFromJson(json)
    c.Assert(exp.Fields[0].Value, Equals, uint32(47))
    c.Assert(exp.Fields[1].Value, Equals, uint32(185))
}

func (s *DeviceSuite) TestComplexStructureFromJson(c *C) {

    stMap := NewStruct("mapstruct")
    Global.BindTo(SymbolWithName("mapstruct"), ObjectWithTypeAndValue("DeviceStructure", unsafe.Pointer(stMap)))
    stMap.AddField(NewField("f1", "uint8", 1, 2, nil, nil))
    stMap.AddField(NewField("f2", "uint8", 1, 1, nil, nil))

    stTest := NewStruct("test")
    Global.BindTo(SymbolWithName("test"), ObjectWithTypeAndValue("DeviceStructure", unsafe.Pointer(stTest)))

    stTest.AddField(NewField("map", "mapstruct", 1, 1, nil, nil))
    stTest.AddField(NewField("f3", "uint8", 1, 1, nil, nil))

    exp := stTest.Expand()

    json := `{"map": {"f1": [47, 75], "f2": 185}, "f3": 85}`

    exp.PopulateFromJson(json)
    c.Assert(exp.Fields[0].Value, Equals, uint32(47))
    c.Assert(exp.Fields[1].Value, Equals, uint32(75))
    c.Assert(exp.Fields[2].Value, Equals, uint32(185))
    c.Assert(exp.Fields[3].Value, Equals, uint32(85))
}

func (s *DeviceSuite) TestComplexStructureFromBytes(c *C) {
    bytes := []byte{47, 75, 185, 85}

    stMap := NewStruct("mapstruct")
    Global.BindTo(SymbolWithName("mapstruct"), ObjectWithTypeAndValue("DeviceStructure", unsafe.Pointer(stMap)))
    stMap.AddField(NewField("f1", "uint8", 1, 2, nil, nil))
    stMap.AddField(NewField("f2", "uint8", 1, 1, nil, nil))

    stTest := NewStruct("test")
    Global.BindTo(SymbolWithName("test"), ObjectWithTypeAndValue("DeviceStructure", unsafe.Pointer(stTest)))

    stTest.AddField(NewField("map", "mapstruct", 1, 1, nil, nil))
    stTest.AddField(NewField("f3", "uint8", 1, 1, nil, nil))

    exp := stTest.Expand()

    exp.PopulateFromBytes(&bytes)

    c.Assert(exp.Fields[0].Value, Equals, uint32(47))
    c.Assert(exp.Fields[1].Value, Equals, uint32(75))
    c.Assert(exp.Fields[2].Value, Equals, uint32(185))
    c.Assert(exp.Fields[3].Value, Equals, uint32(85))
}

func (s *DeviceSuite) TestComplexStructureWithMultipleTypesFromJson(c *C) {

    stMap := NewStruct("mapstruct")
    Global.BindTo(SymbolWithName("mapstruct"), ObjectWithTypeAndValue("DeviceStructure", unsafe.Pointer(stMap)))
    stMap.AddField(NewField("f1", "uint8", 1, 2, nil, nil))
    stMap.AddField(NewField("f2", "uint32", 1, 1, nil, nil))

    stTest := NewStruct("test")
    Global.BindTo(SymbolWithName("test"), ObjectWithTypeAndValue("DeviceStructure", unsafe.Pointer(stTest)))

    stTest.AddField(NewField("map", "mapstruct", 1, 1, nil, nil))
    stTest.AddField(NewField("f3", "uint8", 1, 1, nil, nil))

    exp := stTest.Expand()

    json := `{"map": {"f1": [47, 75], "f2": 185000}, "f3": 85}`

    exp.PopulateFromJson(json)
    c.Assert(exp.Fields[0].Value, Equals, uint32(47))
    c.Assert(exp.Fields[1].Value, Equals, uint32(75))
    c.Assert(exp.Fields[2].Value, Equals, uint32(185000))
    c.Assert(exp.Fields[3].Value, Equals, uint32(85))
}

func (s *DeviceSuite) TestComplexStructureWithMultipleTypesFromBytes(c *C) {
    bytes := []byte{0x2f, 0x4B, 0x00, 0x00, 0xA8, 0xD2, 0x02, 0x00, 0x55}

    stMap := NewStruct("mapstruct")
    Global.BindTo(SymbolWithName("mapstruct"), ObjectWithTypeAndValue("DeviceStructure", unsafe.Pointer(stMap)))
    stMap.AddField(NewField("f1", "uint8", 1, 2, nil, nil))
    stMap.AddField(NewField("f2", "uint32", 4, 1, nil, nil))

    stTest := NewStruct("test")
    Global.BindTo(SymbolWithName("test"), ObjectWithTypeAndValue("DeviceStructure", unsafe.Pointer(stTest)))

    stTest.AddField(NewField("map", "mapstruct", 1, 1, nil, nil))
    stTest.AddField(NewField("f3", "uint8", 1, 1, nil, nil))

    exp := stTest.Expand()

    exp.PopulateFromBytes(&bytes)

    c.Assert(exp.Fields[0].Value, Equals, uint32(47))
    c.Assert(exp.Fields[1].Value, Equals, uint32(75))
    c.Assert(exp.Fields[2].Value, Equals, uint32(185000))
    c.Assert(exp.Fields[3].Value, Equals, uint32(85))
}

func (s *DeviceSuite) TestGeneratingJson(c *C) {
    bytes := []byte{0x2f, 0x4B, 0x00, 0x00, 0xA8, 0xD2, 0x02, 0x00, 0x55}

    stMap := NewStruct("mapstruct")
    Global.BindTo(SymbolWithName("mapstruct"), ObjectWithTypeAndValue("DeviceStructure", unsafe.Pointer(stMap)))
    stMap.AddField(NewField("f1", "uint8", 1, 2, nil, nil))
    stMap.AddField(NewField("f2", "uint32", 4, 1, nil, nil))

    stTest := NewStruct("test")
    Global.BindTo(SymbolWithName("test"), ObjectWithTypeAndValue("DeviceStructure", unsafe.Pointer(stTest)))

    stTest.AddField(NewField("keyed", "mapstruct", 1, 1, nil, nil))
    stTest.AddField(NewField("f3", "uint8", 1, 1, nil, nil))

    exp := stTest.Expand()
    exp.PopulateFromBytes(&bytes)
    tree := exp.Json()
    c.Assert((tree.(map[string]interface{}))["f3"], Equals, uint32(85))
    branch := (tree.(map[string]interface{}))["keyed"]
    c.Assert((branch.(map[string]interface{}))["f2"], Equals, uint32(185000))
    array := (branch.(map[string]interface{}))["f1"]
    c.Assert((array.([]interface{}))[0], Equals, uint32(47))
    c.Assert((array.([]interface{}))[1], Equals, uint32(75))
}
