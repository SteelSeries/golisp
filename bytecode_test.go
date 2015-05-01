// Copyright 2015 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file tests the bytecode runtime.

package golisp

import (
	"fmt"
	. "gopkg.in/check.v1"
	"unsafe"
)

type BytecodeSuite struct {
}

var _ = Suite(&BytecodeSuite{})

func (s *BytecodeSuite) SetUpSuite(c *C) {
	Global = NewSymbolTableFrameBelow(nil, "Global")
	InitBuiltins()
}

func (s *BytecodeSuite) SetUpTest(c *C) {
	ResetBytecodeRuntime()
}

func addPointer(d *Data, code []uint16) []uint16 {
	ptr := uintptr(unsafe.Pointer(d))
	code = append(code, uint16(ptr&0xFFFF))
	code = append(code, uint16((ptr>>16)&0xFFFF))
	code = append(code, uint16((ptr>>32)&0xFFFF))
	code = append(code, uint16((ptr>>48)&0xFFFF))
	return code
}

func (s *BytecodeSuite) TestShortIntegerConstant(c *C) {
	code := []uint16{0x1042}
	Dissassemble(code)
	_, _ = ExecuteBytecode(code, Global)
	c.Assert(BytecodeStackPointer, Equals, 1)
	data := BytecodeDataStack[0]
	c.Assert(int(TypeOf(data)), Equals, IntegerType)
	c.Assert(IntegerValue(data), Equals, int64(0x42))
}

func (s *BytecodeSuite) TestAnotherShortIntegerConstant(c *C) {
	code := []uint16{0x1015}
	Dissassemble(code)
	_, _ = ExecuteBytecode(code, Global)
	c.Assert(BytecodeStackPointer, Equals, 1)
	data := BytecodeDataStack[0]
	c.Assert(int(TypeOf(data)), Equals, IntegerType)
	c.Assert(IntegerValue(data), Equals, int64(0x15))
}

func (s *BytecodeSuite) TestShortFalseConstant(c *C) {
	code := []uint16{0x1100}
	Dissassemble(code)
	_, _ = ExecuteBytecode(code, Global)
	c.Assert(BytecodeStackPointer, Equals, 1)
	data := BytecodeDataStack[0]
	c.Assert(int(TypeOf(data)), Equals, BooleanType)
	c.Assert(BooleanValue(data), Equals, false)
}

func (s *BytecodeSuite) TestShortTrueConstant(c *C) {
	code := []uint16{0x1101}
	Dissassemble(code)
	_, _ = ExecuteBytecode(code, Global)
	c.Assert(BytecodeStackPointer, Equals, 1)
	data := BytecodeDataStack[0]
	c.Assert(int(TypeOf(data)), Equals, BooleanType)
	c.Assert(BooleanValue(data), Equals, true)
}

func (s *BytecodeSuite) TestLongConstant(c *C) {
	constant := SymbolWithName("symbol")
	code := make([]uint16, 0, 5)
	code = append(code, 0x1801)
	code = addPointer(constant, code)
	Dissassemble(code)
	_, _ = ExecuteBytecode(code, Global)
	c.Assert(BytecodeStackPointer, Equals, 1)
	data := BytecodeDataStack[0]
	c.Assert(int(TypeOf(data)), Equals, SymbolType)
	c.Assert(StringValue(data), Equals, "symbol")
}

func (s *BytecodeSuite) TestComplexConstant(c *C) {
	constant, _ := ParseAndEval("'(1 2 3)")
	code := make([]uint16, 0, 5)
	code = append(code, 0x1801)
	code = addPointer(constant, code)
	Dissassemble(code)
	_, _ = ExecuteBytecode(code, Global)
	c.Assert(BytecodeStackPointer, Equals, 1)
	data := BytecodeDataStack[0]
	c.Assert(int(TypeOf(data)), Equals, ConsCellType)
	c.Assert(String(data), Equals, "(1 2 3)")
}

func (s *BytecodeSuite) TestReturn(c *C) {
	code := []uint16{0x1042, 0x3000}
	Dissassemble(code)
	result, _ := ExecuteBytecode(code, Global)
	c.Assert(BytecodeStackPointer, Equals, 0)
	c.Assert(int(TypeOf(result)), Equals, IntegerType)
	c.Assert(IntegerValue(result), Equals, int64(0x42))
}

func (s *BytecodeSuite) TestVarRef(c *C) {
	_, _ = ParseAndEval("(define answer 42)")
	constant := SymbolWithName("answer")
	code := []uint16{0x2000}
	code = addPointer(constant, code)
	code = append(code, 0x3000)
	Dissassemble(code)
	result, _ := ExecuteBytecode(code, Global)
	c.Assert(BytecodeStackPointer, Equals, 0)
	c.Assert(int(TypeOf(result)), Equals, IntegerType)
	c.Assert(IntegerValue(result), Equals, int64(42))
}

// (+ 66 1)
func (s *BytecodeSuite) TestPrimitiveCall(c *C) {
	code := []uint16{0x1042, 0x1001, 0x0200}
	code = addPointer(SymbolWithName("+"), code)
	code = append(code, 0x3000)
	Dissassemble(code)
	result, _ := ExecuteBytecode(code, Global)
	c.Assert(BytecodeStackPointer, Equals, 0)
	c.Assert(int(TypeOf(result)), Equals, IntegerType)
	c.Assert(IntegerValue(result), Equals, int64(67))
}

// (define (foo x y) (+ x y))
// (foo 66 1)
func (s *BytecodeSuite) TestFunctionCall(c *C) {
	ParseAndEval("(define (foo x y) (+ x y))")
	code := []uint16{0x1042, 0x1001, 0x0200}
	code = addPointer(SymbolWithName("foo"), code)
	code = append(code, 0x3000)
	Dissassemble(code)
	result, _ := ExecuteBytecode(code, Global)
	c.Assert(BytecodeStackPointer, Equals, 0)
	c.Assert(int(TypeOf(result)), Equals, IntegerType)
	c.Assert(IntegerValue(result), Equals, int64(67))
}

func (s *BytecodeSuite) TestCompiledFunctionCall(c *C) {
	// compiled (define (foo x y) (+ x y))
	// VARREF x
	fcode := []uint16{0x2000}
	fcode = addPointer(SymbolWithName("x"), fcode)
	// VARREF y
	fcode = append(fcode, 0x2000)
	fcode = addPointer(SymbolWithName("y"), fcode)
	// CALL +, 2
	fcode = append(fcode, 0x0200)
	fcode = addPointer(SymbolWithName("+"), fcode)
	// RETURN
	fcode = append(fcode, 0x3000)
	fmt.Printf("compiled function body\n")
	Dissassemble(fcode)

	argList, _ := Parse("(x y)")
	f := CompiledFunctionWithNameParamsBodyAndParent("foo", argList, fcode, Global)
	Global.BindTo(SymbolWithName("foo"), f)

	code := []uint16{0x1042, 0x1001, 0x0200}
	code = addPointer(SymbolWithName("foo"), code)
	code = append(code, 0x3000)
	Dissassemble(code)
	result, _ := ExecuteBytecode(code, Global)
	c.Assert(BytecodeStackPointer, Equals, 0)
	c.Assert(int(TypeOf(result)), Equals, IntegerType)
	c.Assert(IntegerValue(result), Equals, int64(67))
}

func (s *BytecodeSuite) TestShortBranchOnTrue(c *C) {
	// 0000: CONSTANT# true
	// 0001: BRAST 2
	// 0002: CONSTANT# 0
	// 0003: RETURN
	// 0004: CONSTANT# 1
	// 0005: RETURN
	code := []uint16{0x1101, 0x4402, 0x1000, 0x3000, 0x1001, 0x3000}
	Dissassemble(code)
	result, _ := ExecuteBytecode(code, Global)
	c.Assert(int(TypeOf(result)), Equals, IntegerType)
	c.Assert(IntegerValue(result), Equals, int64(1))
}

func (s *BytecodeSuite) TestNotShortBranchOnTrue(c *C) {
	// 0000: CONSTANT# false
	// 0001: BRAST 2
	// 0002: CONSTANT# 0
	// 0003: RETURN
	// 0004: CONSTANT# 1
	// 0005: RETURN
	code := []uint16{0x1100, 0x4402, 0x1000, 0x3000, 0x1001, 0x3000}
	Dissassemble(code)
	result, _ := ExecuteBytecode(code, Global)
	c.Assert(int(TypeOf(result)), Equals, IntegerType)
	c.Assert(IntegerValue(result), Equals, int64(0))
}

func (s *BytecodeSuite) TestShortBranchOnFalse(c *C) {
	// 0000: CONSTANT# false
	// 0001: BRASF 2
	// 0002: CONSTANT# 0
	// 0003: RETURN
	// 0004: CONSTANT# 1
	// 0005: RETURN
	code := []uint16{0x1100, 0x4202, 0x1000, 0x3000, 0x1001, 0x3000}
	Dissassemble(code)
	result, _ := ExecuteBytecode(code, Global)
	c.Assert(int(TypeOf(result)), Equals, IntegerType)
	c.Assert(IntegerValue(result), Equals, int64(1))
}

func (s *BytecodeSuite) TestNotShortBranchOnFalse(c *C) {
	// 0000: CONSTANT# true
	// 0001: BRASF 2
	// 0002: CONSTANT# 0
	// 0003: RETURN
	// 0004: CONSTANT# 1
	// 0005: RETURN
	code := []uint16{0x1101, 0x4202, 0x1000, 0x3000, 0x1001, 0x3000}
	Dissassemble(code)
	result, _ := ExecuteBytecode(code, Global)
	c.Assert(int(TypeOf(result)), Equals, IntegerType)
	c.Assert(IntegerValue(result), Equals, int64(0))
}

func (s *BytecodeSuite) TestShortBranch(c *C) {
	// 0000: CONSTANT# false
	// 0001: BRAS 2
	// 0002: CONSTANT# 0
	// 0003: RETURN
	// 0004: CONSTANT# 1
	// 0005: RETURN
	code := []uint16{0x1100, 0x4002, 0x1000, 0x3000, 0x1001, 0x3000}
	Dissassemble(code)
	result, _ := ExecuteBytecode(code, Global)
	c.Assert(int(TypeOf(result)), Equals, IntegerType)
	c.Assert(IntegerValue(result), Equals, int64(1))
}

func (s *BytecodeSuite) TestOtherShortBranch(c *C) {
	// 0000: CONSTANT# true
	// 0001: BRAS 2
	// 0002: CONSTANT# 0
	// 0003: RETURN
	// 0004: CONSTANT# 1
	// 0005: RETURN
	code := []uint16{0x1101, 0x4002, 0x1000, 0x3000, 0x1001, 0x3000}
	Dissassemble(code)
	result, _ := ExecuteBytecode(code, Global)
	c.Assert(int(TypeOf(result)), Equals, IntegerType)
	c.Assert(IntegerValue(result), Equals, int64(1))
}

func (s *BytecodeSuite) TestLongBranchOnTrue(c *C) {
	// 0000: CONSTANT# true
	// 0001: BRALT 2
	// 0002: CONSTANT# 0
	// 0003: RETURN
	// 0004: CONSTANT# 1
	// 0005: RETURN
	code := []uint16{0x1101, 0x4C00, 0x0002, 0x1000, 0x3000, 0x1001, 0x3000}
	Dissassemble(code)
	result, _ := ExecuteBytecode(code, Global)
	c.Assert(int(TypeOf(result)), Equals, IntegerType)
	c.Assert(IntegerValue(result), Equals, int64(1))
}

func (s *BytecodeSuite) TestNotLongBranchOnTrue(c *C) {
	// 0000: CONSTANT# false
	// 0001: BRALT 2
	// 0002: CONSTANT# 0
	// 0003: RETURN
	// 0004: CONSTANT# 1
	// 0005: RETURN
	code := []uint16{0x1100, 0x4C02, 0x0002, 0x1000, 0x3000, 0x1001, 0x3000}
	Dissassemble(code)
	result, _ := ExecuteBytecode(code, Global)
	c.Assert(int(TypeOf(result)), Equals, IntegerType)
	c.Assert(IntegerValue(result), Equals, int64(0))
}

func (s *BytecodeSuite) TestLongBranchOnFalse(c *C) {
	// 0000: CONSTANT# false
	// 0001: BRALF 2
	// 0002: CONSTANT# 0
	// 0003: RETURN
	// 0004: CONSTANT# 1
	// 0005: RETURN
	code := []uint16{0x1100, 0x4A02, 0x0002, 0x1000, 0x3000, 0x1001, 0x3000}
	Dissassemble(code)
	result, _ := ExecuteBytecode(code, Global)
	c.Assert(int(TypeOf(result)), Equals, IntegerType)
	c.Assert(IntegerValue(result), Equals, int64(1))
}

func (s *BytecodeSuite) TestNotLongBranchOnFalse(c *C) {
	// 0000: CONSTANT# true
	// 0001: BRALF 2
	// 0002: CONSTANT# 0
	// 0003: RETURN
	// 0004: CONSTANT# 1
	// 0005: RETURN
	code := []uint16{0x1101, 0x4A02, 0x0002, 0x1000, 0x3000, 0x1001, 0x3000}
	Dissassemble(code)
	result, _ := ExecuteBytecode(code, Global)
	c.Assert(int(TypeOf(result)), Equals, IntegerType)
	c.Assert(IntegerValue(result), Equals, int64(0))
}

func (s *BytecodeSuite) TestLongBranch(c *C) {
	// 0000: CONSTANT# false
	// 0001: BRAL 2
	// 0002: CONSTANT# 0
	// 0003: RETURN
	// 0004: CONSTANT# 1
	// 0005: RETURN
	code := []uint16{0x1100, 0x4800, 0x0002, 0x1000, 0x3000, 0x1001, 0x3000}
	Dissassemble(code)
	result, _ := ExecuteBytecode(code, Global)
	c.Assert(int(TypeOf(result)), Equals, IntegerType)
	c.Assert(IntegerValue(result), Equals, int64(1))
}

func (s *BytecodeSuite) TestOtherLongBranch(c *C) {
	// 0000: CONSTANT# true
	// 0001: BRAL 2
	// 0002: CONSTANT# 0
	// 0003: RETURN
	// 0004: CONSTANT# 1
	// 0005: RETURN
	code := []uint16{0x1101, 0x4802, 0x0002, 0x1000, 0x3000, 0x1001, 0x3000}
	Dissassemble(code)
	result, _ := ExecuteBytecode(code, Global)
	c.Assert(int(TypeOf(result)), Equals, IntegerType)
	c.Assert(IntegerValue(result), Equals, int64(1))
}
