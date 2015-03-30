// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file tests bytearray built-ins.

package golisp

import (
	. "launchpad.net/gocheck"
	"unsafe"
)

type BytearrayBuiltinsSuite struct {
}

var _ = Suite(&BytearrayBuiltinsSuite{})

func (s *BytearrayBuiltinsSuite) SetUpSuite(c *C) {
	Global = NewSymbolTableFrameBelow(nil, "SystemGlobal")
	InitBuiltins()
}

//--------------------------------------------------------------------------------
// ListToBytes

func (s *BytearrayBuiltinsSuite) TestListToBytes(c *C) {
	source := "'(1 2 3 4 5 6)"
	list, err := Parse(source)
	c.Assert(err, IsNil)
	c.Assert(list, NotNil)
	b, err := ListToBytesImpl(InternalMakeList(list), Global)
	c.Assert(err, IsNil)
	c.Assert(ObjectType(b), Equals, "[]byte")
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
	_, err = ListToBytesImpl(InternalMakeList(list), Global)
	c.Assert(err, NotNil)
}

func (s *BytearrayBuiltinsSuite) TestListToBytesWithNonnumericValues(c *C) {
	source := "'(1 foo 3)"
	list, err := Parse(source)
	c.Assert(err, IsNil)
	c.Assert(list, NotNil)
	_, err = ListToBytesImpl(InternalMakeList(list), Global)
	c.Assert(err, NotNil)
}

func (s *BytearrayBuiltinsSuite) TestListToBytesWithNonList(c *C) {
	_, err := ListToBytesImpl(IntegerWithValue(42), Global)
	c.Assert(err, NotNil)
}

func (s *BytearrayBuiltinsSuite) TestListToBytesWithNil(c *C) {
	_, err := ListToBytesImpl(nil, Global)
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
	l, err := BytesToListImpl(InternalMakeList(o), Global)
	c.Assert(err, IsNil)
	c.Assert(l, NotNil)
	for i, cell := 1, l; NotNilP(cell); i, cell = i+1, Cdr(cell) {
		c.Assert(IntegerValue(Car(cell)), Equals, int64(i))
	}
}

func (s *BytearrayBuiltinsSuite) TestBytesToListWithNonObject(c *C) {
	_, err := BytesToListImpl(IntegerWithValue(42), Global)
	c.Assert(err, NotNil)
}

func (s *BytearrayBuiltinsSuite) TestBytesToListWithNil(c *C) {
	_, err := BytesToListImpl(nil, Global)
	c.Assert(err, NotNil)
}

func (s *BytearrayBuiltinsSuite) TestBytesToListWithNonBytearrayObject(c *C) {
	o := ObjectWithTypeAndValue("nil", unsafe.Pointer(nil))
	_, err := BytesToListImpl(InternalMakeList(o), Global)
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
	r, err := ReplaceByteImpl(InternalMakeList(o, IntegerWithValue(3), IntegerWithValue(0xaa)), Global)
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
	_, err := ReplaceByteImpl(InternalMakeList(nil, IntegerWithValue(3), IntegerWithValue(0xaa)), Global)
	c.Assert(err, NotNil)
}

func (s *BytearrayBuiltinsSuite) TestReplaceByteWithNilSecondArg(c *C) {
	dataBytes := make([]byte, 5)
	for i := 0; i < 5; i++ {
		dataBytes[i] = byte(i + 1)
	}
	o := ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&dataBytes))
	_, err := ReplaceByteImpl(InternalMakeList(o, nil, IntegerWithValue(0xaa)), Global)
	c.Assert(err, NotNil)
}

func (s *BytearrayBuiltinsSuite) TestReplaceByteWithNilThirdArg(c *C) {
	dataBytes := make([]byte, 5)
	for i := 0; i < 5; i++ {
		dataBytes[i] = byte(i + 1)
	}
	o := ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&dataBytes))
	_, err := ReplaceByteImpl(InternalMakeList(o, IntegerWithValue(3), nil), Global)
	c.Assert(err, NotNil)
}

func (s *BytearrayBuiltinsSuite) TestReplaceByteWithIndexOutOfRange(c *C) {
	dataBytes := make([]byte, 5)
	for i := 0; i < 5; i++ {
		dataBytes[i] = byte(i + 1)
	}
	o := ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&dataBytes))
	_, err := ReplaceByteImpl(InternalMakeList(o, IntegerWithValue(10), IntegerWithValue(0xaa)), Global)
	c.Assert(err, NotNil)
}

func (s *BytearrayBuiltinsSuite) TestReplaceByteWithValueNotByte(c *C) {
	dataBytes := make([]byte, 5)
	for i := 0; i < 5; i++ {
		dataBytes[i] = byte(i + 1)
	}
	o := ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&dataBytes))
	_, err := ReplaceByteImpl(InternalMakeList(o, IntegerWithValue(10), IntegerWithValue(300)), Global)
	c.Assert(err, NotNil)
}

func (s *BytearrayBuiltinsSuite) TestReplaceByteBang(c *C) {
	dataBytes := make([]byte, 5)
	for i := 0; i < 5; i++ {
		dataBytes[i] = byte(i + 1)
	}
	o := ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&dataBytes))
	r, err := ReplaceByteBangImpl(InternalMakeList(o, IntegerWithValue(3), IntegerWithValue(0xaa)), Global)
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

	for i := 0; i < 5; i++ {
		b, err := ExtractByteImpl(InternalMakeList(o, IntegerWithValue(int64(i))), Global)
		c.Assert(err, IsNil)
		c.Assert(b, NotNil)
		c.Assert(IntegerP(b), Equals, true)
		c.Assert(IntegerValue(b), Equals, int64(i+1))
	}
}

func (s *BytearrayBuiltinsSuite) TestExtractByteWithNilFirstArg(c *C) {
	_, err := ExtractByteImpl(InternalMakeList(nil, IntegerWithValue(3)), Global)
	c.Assert(err, NotNil)
}

func (s *BytearrayBuiltinsSuite) TestExtractByteWithNilSecondArg(c *C) {
	dataBytes := make([]byte, 5)
	for i := 0; i < 5; i++ {
		dataBytes[i] = byte(i + 1)
	}
	o := ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&dataBytes))
	_, err := ExtractByteImpl(InternalMakeList(o, nil), Global)
	c.Assert(err, NotNil)
}

func (s *BytearrayBuiltinsSuite) TestExtractByteWithIndexOutOfRange(c *C) {
	dataBytes := make([]byte, 5)
	for i := 0; i < 5; i++ {
		dataBytes[i] = byte(i + 1)
	}
	o := ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&dataBytes))
	_, err := ExtractByteImpl(InternalMakeList(o, IntegerWithValue(10)), Global)
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
	r, err := AppendBytesImpl(InternalMakeList(o, IntegerWithValue(6)), Global)
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
	r, err := AppendBytesImpl(InternalMakeList(o, IntegerWithValue(6), IntegerWithValue(7), IntegerWithValue(8)), Global)
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
	r, err := AppendBytesImpl(InternalMakeList(o, InternalMakeList(SymbolWithName("quote"), InternalMakeList(IntegerWithValue(6), IntegerWithValue(7), IntegerWithValue(8)))), Global)
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
	r, err := AppendBytesImpl(InternalMakeList(o, InternalMakeList(SymbolWithName("list"), IntegerWithValue(6), IntegerWithValue(7), IntegerWithValue(8))), Global)
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

	r, err := AppendBytesImpl(InternalMakeList(o, o2), Global)
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
	r, err := AppendBytesBangImpl(InternalMakeList(o, InternalMakeList(SymbolWithName("list"), IntegerWithValue(6), IntegerWithValue(7), IntegerWithValue(8))), Global)
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

//--------------------------------------------------------------------------------
// Take

func (s *BytearrayBuiltinsSuite) TestTakeByteArray(c *C) {
	dataBytes := make([]byte, 5)
	for i := 0; i < 5; i++ {
		dataBytes[i] = byte(i + 1)
	}
	o := ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&dataBytes))
	r, err := TakeImpl(InternalMakeList(IntegerWithValue(3), o), Global)
	c.Assert(err, IsNil)
	c.Assert(r, NotNil)

	changedBytes := (*[]byte)(ObjectValue(r))
	c.Assert(changedBytes, NotNil)
	c.Assert(len(*changedBytes), Equals, 3)
	for i, d := range *changedBytes {
		c.Assert(d, Equals, byte(i+1))
	}
}

func (s *BytearrayBuiltinsSuite) TestTakeByteArrayIndexGreaterThanLength(c *C) {
	dataBytes := make([]byte, 5)
	for i := 0; i < 5; i++ {
		dataBytes[i] = byte(i + 1)
	}
	o := ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&dataBytes))
	r, err := TakeImpl(InternalMakeList(IntegerWithValue(8), o), Global)
	c.Assert(err, IsNil)
	c.Assert(r, NotNil)

	changedBytes := (*[]byte)(ObjectValue(r))
	c.Assert(changedBytes, NotNil)
	c.Assert(len(*changedBytes), Equals, 5)
	for i, d := range *changedBytes {
		c.Assert(d, Equals, byte(i+1))
	}
}

//--------------------------------------------------------------------------------
// Drop

func (s *BytearrayBuiltinsSuite) TestDropByteArray(c *C) {
	dataBytes := make([]byte, 5)
	for i := 0; i < 5; i++ {
		dataBytes[i] = byte(i + 1)
	}
	o := ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&dataBytes))
	r, err := DropImpl(InternalMakeList(IntegerWithValue(2), o), Global)
	c.Assert(err, IsNil)
	c.Assert(r, NotNil)

	changedBytes := (*[]byte)(ObjectValue(r))
	c.Assert(changedBytes, NotNil)
	c.Assert(len(*changedBytes), Equals, 3)
	for i, d := range *changedBytes {
		c.Assert(d, Equals, byte(i+3))
	}
}

func (s *BytearrayBuiltinsSuite) TestDropByteArrayIndexGreaterThanLength(c *C) {
	dataBytes := make([]byte, 5)
	for i := 0; i < 5; i++ {
		dataBytes[i] = byte(i + 1)
	}
	o := ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&dataBytes))
	r, err := DropImpl(InternalMakeList(IntegerWithValue(8), o), Global)
	c.Assert(err, IsNil)
	c.Assert(r, NotNil)

	changedBytes := (*[]byte)(ObjectValue(r))
	c.Assert(changedBytes, NotNil)
	c.Assert(len(*changedBytes), Equals, 0)
}

//--------------------------------------------------------------------------------
// ExtractBytes
//  (TODO)
func (s *BytearrayBuiltinsSuite) TestExtractBytes(c *C) {
	dataBytes := make([]byte, 5)
	for i := 0; i < 5; i++ {
		dataBytes[i] = byte(i + 1)
	}
	o := ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&dataBytes))

	r, err := ExtractBytesImpl(InternalMakeList(o, IntegerWithValue(2), IntegerWithValue(3)), Global)
	c.Assert(err, IsNil)
	c.Assert(r, NotNil)

	changedBytes := (*[]byte)(ObjectValue(r))
	c.Assert(changedBytes, NotNil)
	c.Assert(len(*changedBytes), Equals, 3)
	for i, d := range *changedBytes {
		c.Assert(d, Equals, byte(i+3))
	}
}

func (s *BytearrayBuiltinsSuite) TestExtractBytesWithNilFirstArg(c *C) {
	_, err := ExtractBytesImpl(InternalMakeList(nil, IntegerWithValue(3), IntegerWithValue(2)), Global)
	c.Assert(err, NotNil)
}

func (s *BytearrayBuiltinsSuite) TestExtractBytesWithNilSecondArg(c *C) {
	dataBytes := make([]byte, 5)
	for i := 0; i < 5; i++ {
		dataBytes[i] = byte(i + 1)
	}
	o := ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&dataBytes))
	_, err := ExtractBytesImpl(InternalMakeList(o, nil, IntegerWithValue(2)), Global)
	c.Assert(err, NotNil)
}

func (s *BytearrayBuiltinsSuite) TestExtractBytesWithNilThirdArg(c *C) {
	dataBytes := make([]byte, 5)
	for i := 0; i < 5; i++ {
		dataBytes[i] = byte(i + 1)
	}
	o := ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&dataBytes))
	_, err := ExtractBytesImpl(InternalMakeList(o, IntegerWithValue(3), nil), Global)
	c.Assert(err, NotNil)
}

func (s *BytearrayBuiltinsSuite) TestExtractBytesWithIndexOutOfRange(c *C) {
	dataBytes := make([]byte, 5)
	for i := 0; i < 5; i++ {
		dataBytes[i] = byte(i + 1)
	}
	o := ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&dataBytes))
	_, err := ExtractBytesImpl(InternalMakeList(o, IntegerWithValue(10), IntegerWithValue(1)), Global)
	c.Assert(err, NotNil)
}

func (s *BytearrayBuiltinsSuite) TestExtractBytesWithFinalIndexOutOfRange(c *C) {
	dataBytes := make([]byte, 5)
	for i := 0; i < 5; i++ {
		dataBytes[i] = byte(i + 1)
	}
	o := ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&dataBytes))
	_, err := ExtractBytesImpl(InternalMakeList(o, IntegerWithValue(1), IntegerWithValue(10)), Global)
	c.Assert(err, NotNil)
}
