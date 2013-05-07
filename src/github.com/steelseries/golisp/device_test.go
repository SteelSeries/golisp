// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file tests device description and serialization
package golisp

import (
    . "launchpad.net/gocheck"
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
