// Copyright 2013 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file tests bytearray builtins

package golisp

import (
    . "launchpad.net/gocheck"
    "unsafe"
)

type BytearrayBuiltinsSuite struct {
}

var _ = Suite(&BytearrayBuiltinsSuite{})

func (s *BytearrayBuiltinsSuite) SetUpSuite(c *C) {
    Global = NewSymbolTableFrameBelow(nil)
    InitBuiltins()
}

//--------------------------------------------------------------------------------
// ListToBytes

func (s *BytearrayBuiltinsSuite) TestListToBytes(c *C) {
    source := "'(1 2 3 4 5 6)"
    list, err := Parse(source)
    c.Assert(err, IsNil)
    c.Assert(list, NotNil)
    b, err := ListToBytes(InternalMakeList(list), Global)
    c.Assert(err, IsNil)
    c.Assert(b.ObjType, Equals, "[]byte")
    dataBytes := (*[]byte)(ObjectValue(b))
    c.Assert(dataBytes, NotNil)
    for i, d := range *dataBytes {
        c.Assert(d, Equals, byte(i+1))
    }
}

func (s *BytearrayBuiltinsSuite) TestListToBytesWithBadValues(c *C) {
    source := "'(1 256 3)"
    list, err := Parse(source)
    c.Assert(err, IsNil)
    c.Assert(list, NotNil)
    _, err = ListToBytes(InternalMakeList(list), Global)
    c.Assert(err, NotNil)
}

func (s *BytearrayBuiltinsSuite) TestListToBytesWithNonnumericValues(c *C) {
    source := "'(1 foo 3)"
    list, err := Parse(source)
    c.Assert(err, IsNil)
    c.Assert(list, NotNil)
    _, err = ListToBytes(InternalMakeList(list), Global)
    c.Assert(err, NotNil)
}

func (s *BytearrayBuiltinsSuite) TestListToBytesWithNonList(c *C) {
    _, err := ListToBytes(NumberWithValue(42), Global)
    c.Assert(err, NotNil)
}

func (s *BytearrayBuiltinsSuite) TestListToBytesWithNil(c *C) {
    _, err := ListToBytes(nil, Global)
    c.Assert(err, NotNil)
}

//--------------------------------------------------------------------------------
// BytesToList

func (s *BytearrayBuiltinsSuite) TestBytesToList(c *C) {
    dataBytes := make([]byte, 5)
    for i := 0; i < 5; i++ {
        dataBytes[i] = byte(i + 1)
    }
    o := ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&dataBytes))
    l, err := BytesToList(InternalMakeList(o), Global)
    c.Assert(err, IsNil)
    c.Assert(l, NotNil)
    for i, cell := uint32(1), l; NotNilP(cell); i, cell = i+1, Cdr(cell) {
        c.Assert(NumericValue(Car(cell)), Equals, i)
    }
}

func (s *BytearrayBuiltinsSuite) TestBytesToListWithNonObject(c *C) {
    _, err := BytesToList(NumberWithValue(42), Global)
    c.Assert(err, NotNil)
}

func (s *BytearrayBuiltinsSuite) TestBytesToListWithNil(c *C) {
    _, err := BytesToList(nil, Global)
    c.Assert(err, NotNil)
}

func (s *BytearrayBuiltinsSuite) TestBytesToListWithNonBytearrayObject(c *C) {
    o := ObjectWithTypeAndValue("nil", unsafe.Pointer(nil))
    _, err := BytesToList(InternalMakeList(o), Global)
    c.Assert(err, NotNil)
}

//--------------------------------------------------------------------------------
// ReplaceByte

func (s *BytearrayBuiltinsSuite) TestReplaceByte(c *C) {
    dataBytes := make([]byte, 5)
    for i := 0; i < 5; i++ {
        dataBytes[i] = byte(i + 1)
    }
    o := ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&dataBytes))
    r, err := ReplaceByte(InternalMakeList(o, NumberWithValue(3), NumberWithValue(0xaa)), Global)
    c.Assert(err, IsNil)
    c.Assert(r, NotNil)

    changedBytes := (*[]byte)(ObjectValue(r))
    c.Assert(changedBytes, NotNil)
    for i, d := range *changedBytes {
        if i == 3 {
            c.Assert(d, Equals, byte(0xaa))
        } else {
            c.Assert(d, Equals, byte(i+1))
        }
    }
}

func (s *BytearrayBuiltinsSuite) TestReplaceByteWithNilFirstArg(c *C) {
    _, err := ReplaceByte(InternalMakeList(nil, NumberWithValue(3), NumberWithValue(0xaa)), Global)
    c.Assert(err, NotNil)
}

func (s *BytearrayBuiltinsSuite) TestReplaceByteWithNilSecondArg(c *C) {
    dataBytes := make([]byte, 5)
    for i := 0; i < 5; i++ {
        dataBytes[i] = byte(i + 1)
    }
    o := ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&dataBytes))
    _, err := ReplaceByte(InternalMakeList(o, nil, NumberWithValue(0xaa)), Global)
    c.Assert(err, NotNil)
}

func (s *BytearrayBuiltinsSuite) TestReplaceByteWithNilThirdArg(c *C) {
    dataBytes := make([]byte, 5)
    for i := 0; i < 5; i++ {
        dataBytes[i] = byte(i + 1)
    }
    o := ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&dataBytes))
    _, err := ReplaceByte(InternalMakeList(o, NumberWithValue(3), nil), Global)
    c.Assert(err, NotNil)
}

func (s *BytearrayBuiltinsSuite) TestReplaceByteWithIndexOutOfRange(c *C) {
    dataBytes := make([]byte, 5)
    for i := 0; i < 5; i++ {
        dataBytes[i] = byte(i + 1)
    }
    o := ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&dataBytes))
    _, err := ReplaceByte(InternalMakeList(o, NumberWithValue(10), NumberWithValue(0xaa)), Global)
    c.Assert(err, NotNil)
}

func (s *BytearrayBuiltinsSuite) TestReplaceByteWithValueNotByte(c *C) {
    dataBytes := make([]byte, 5)
    for i := 0; i < 5; i++ {
        dataBytes[i] = byte(i + 1)
    }
    o := ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&dataBytes))
    _, err := ReplaceByte(InternalMakeList(o, NumberWithValue(10), NumberWithValue(300)), Global)
    c.Assert(err, NotNil)
}

func (s *BytearrayBuiltinsSuite) TestReplaceByteBang(c *C) {
    dataBytes := make([]byte, 5)
    for i := 0; i < 5; i++ {
        dataBytes[i] = byte(i + 1)
    }
    o := ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&dataBytes))
    r, err := ReplaceByteBang(InternalMakeList(o, NumberWithValue(3), NumberWithValue(0xaa)), Global)
    c.Assert(err, IsNil)
    c.Assert(r, NotNil)
    c.Assert(r, Equals, o)

    changedBytes := (*[]byte)(ObjectValue(r))
    c.Assert(changedBytes, NotNil)
    for i, d := range *changedBytes {
        if i == 3 {
            c.Assert(d, Equals, byte(0xaa))
        } else {
            c.Assert(d, Equals, byte(i+1))
        }
    }
}

//--------------------------------------------------------------------------------
// ExtractByte

func (s *BytearrayBuiltinsSuite) TestExtractByte(c *C) {
    dataBytes := make([]byte, 5)
    for i := 0; i < 5; i++ {
        dataBytes[i] = byte(i + 1)
    }
    o := ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&dataBytes))

    for i := uint32(0); i < 5; i++ {
        b, err := ExtractByte(InternalMakeList(o, NumberWithValue(i)), Global)
        c.Assert(err, IsNil)
        c.Assert(b, NotNil)
        c.Assert(NumberP(b), Equals, true)
        c.Assert(NumericValue(b), Equals, i+1)
    }
}

func (s *BytearrayBuiltinsSuite) TestExtractByteWithNilFirstArg(c *C) {
    _, err := ExtractByte(InternalMakeList(nil, NumberWithValue(3)), Global)
    c.Assert(err, NotNil)
}

func (s *BytearrayBuiltinsSuite) TestExtractByteWithNilSecondArg(c *C) {
    dataBytes := make([]byte, 5)
    for i := 0; i < 5; i++ {
        dataBytes[i] = byte(i + 1)
    }
    o := ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&dataBytes))
    _, err := ExtractByte(InternalMakeList(o, nil), Global)
    c.Assert(err, NotNil)
}

func (s *BytearrayBuiltinsSuite) TestExtractByteWithIndexOutOfRange(c *C) {
    dataBytes := make([]byte, 5)
    for i := 0; i < 5; i++ {
        dataBytes[i] = byte(i + 1)
    }
    o := ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&dataBytes))
    _, err := ExtractByte(InternalMakeList(o, NumberWithValue(10)), Global)
    c.Assert(err, NotNil)
}

//--------------------------------------------------------------------------------
// AppendBytes

// (append-bytes b 6)
func (s *BytearrayBuiltinsSuite) TestAppendASingleByte(c *C) {
    dataBytes := make([]byte, 5)
    for i := 0; i < 5; i++ {
        dataBytes[i] = byte(i + 1)
    }
    o := ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&dataBytes))
    r, err := AppendBytes(InternalMakeList(o, NumberWithValue(6)), Global)
    c.Assert(err, IsNil)
    c.Assert(r, NotNil)

    changedBytes := (*[]byte)(ObjectValue(r))
    c.Assert(changedBytes, NotNil)
    c.Assert(len(*changedBytes), Equals, 6)
    for i, d := range *changedBytes {
        c.Assert(d, Equals, byte(i+1))
    }
}

// (append-bytes b 6 7 8)
func (s *BytearrayBuiltinsSuite) TestAppendMultipleBytes(c *C) {
    dataBytes := make([]byte, 5)
    for i := 0; i < 5; i++ {
        dataBytes[i] = byte(i + 1)
    }
    o := ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&dataBytes))
    r, err := AppendBytes(InternalMakeList(o, NumberWithValue(6), NumberWithValue(7), NumberWithValue(8)), Global)
    c.Assert(err, IsNil)
    c.Assert(r, NotNil)

    changedBytes := (*[]byte)(ObjectValue(r))
    c.Assert(changedBytes, NotNil)
    c.Assert(len(*changedBytes), Equals, 8)
    for i, d := range *changedBytes {
        c.Assert(d, Equals, byte(i+1))
    }
}

// (append-bytes b '(6 7 8))
func (s *BytearrayBuiltinsSuite) TestAppendMultipleBytesInAQuotedList(c *C) {
    dataBytes := make([]byte, 5)
    for i := 0; i < 5; i++ {
        dataBytes[i] = byte(i + 1)
    }
    o := ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&dataBytes))
    r, err := AppendBytes(InternalMakeList(o, InternalMakeList(SymbolWithName("quote"), InternalMakeList(NumberWithValue(6), NumberWithValue(7), NumberWithValue(8)))), Global)
    c.Assert(err, IsNil)
    c.Assert(r, NotNil)

    changedBytes := (*[]byte)(ObjectValue(r))
    c.Assert(changedBytes, NotNil)
    c.Assert(len(*changedBytes), Equals, 8)
    for i, d := range *changedBytes {
        c.Assert(d, Equals, byte(i+1))
    }
}

// (append-bytes b (list 6 7 8))
func (s *BytearrayBuiltinsSuite) TestAppendMultipleBytesResultingFromAnSexpr(c *C) {
    dataBytes := make([]byte, 5)
    for i := 0; i < 5; i++ {
        dataBytes[i] = byte(i + 1)
    }
    o := ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&dataBytes))
    r, err := AppendBytes(InternalMakeList(o, InternalMakeList(SymbolWithName("list"), NumberWithValue(6), NumberWithValue(7), NumberWithValue(8))), Global)
    c.Assert(err, IsNil)
    c.Assert(r, NotNil)

    changedBytes := (*[]byte)(ObjectValue(r))
    c.Assert(changedBytes, NotNil)
    c.Assert(len(*changedBytes), Equals, 8)
    for i, d := range *changedBytes {
        c.Assert(d, Equals, byte(i+1))
    }
}

// (append-bytes b b2)
func (s *BytearrayBuiltinsSuite) TestAppendByteArray(c *C) {
    dataBytes := make([]byte, 5)
    for i := 0; i < 5; i++ {
        dataBytes[i] = byte(i + 1)
    }
    o := ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&dataBytes))

    moreDataBytes := make([]byte, 5)
    for i := 0; i < 5; i++ {
        moreDataBytes[i] = byte(i + 6)
    }
    o2 := ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&moreDataBytes))

    r, err := AppendBytes(InternalMakeList(o, o2), Global)
    c.Assert(err, IsNil)
    c.Assert(r, NotNil)

    changedBytes := (*[]byte)(ObjectValue(r))
    c.Assert(changedBytes, NotNil)
    c.Assert(len(*changedBytes), Equals, 10)
    for i, d := range *changedBytes {
        c.Assert(d, Equals, byte(i+1))
    }
}

// (append-bytes! b (list 6 7 8))
func (s *BytearrayBuiltinsSuite) TestAppendInPlaceMultipleBytesResultingFromAnSexpr(c *C) {
    dataBytes := make([]byte, 5)
    for i := 0; i < 5; i++ {
        dataBytes[i] = byte(i + 1)
    }
    o := ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&dataBytes))
    r, err := AppendBytesBang(InternalMakeList(o, InternalMakeList(SymbolWithName("list"), NumberWithValue(6), NumberWithValue(7), NumberWithValue(8))), Global)
    c.Assert(err, IsNil)
    c.Assert(r, NotNil)
    c.Assert(r, Equals, o)

    changedBytes := (*[]byte)(ObjectValue(r))
    c.Assert(changedBytes, NotNil)
    c.Assert(len(*changedBytes), Equals, 8)
    for i, d := range *changedBytes {
        c.Assert(d, Equals, byte(i+1))
    }
}
