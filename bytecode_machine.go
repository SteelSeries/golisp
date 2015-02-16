// Copyright 2015 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file implements the bytecode machine.

package golisp

import (
	"errors"
	"fmt"
	"unsafe"
)

const (
	OPCODE_MASK     = 0xf000
	OPCODE_CALL     = 0x0000
	OPCODE_CONSTANT = 0x1000
	OPCODE_VAR_REF  = 0x2000
	OPCODE_RETURN   = 0x3000
	OPCODE_BRANCH   = 0x4000
)

const (
	FUNCTION_ARITY_MASK  = 0x0F00
	FUNCTION_ARITY_SHIFT = 8
)

const (
	CONSTANT_SIZE_MASK       = 0x0800
	SHORT_CONSTANT           = 0x0000
	LONG_CONSTANT            = 0x0800
	SHORT_CONSTANT_TYPE_MASK = 0x0700
	CONSTANT_INTEGER         = 0x0000
	CONSTANT_BOOLEAN         = 0x0100
	SHORT_CONSTANT_MASK      = 0x00FF
)

const (
	BRANCH_SIZE_MASK         = 0x0800
	SHORT_BRANCH             = 0x0000
	LONG_BRANCH              = 0x0800
	BRANCH_SHORT_OFFSET_MASK = 0x00FF
)

const (
	BRANCH_CONDITION_MASK  = 0x0600
	BRANCH_UNCONDITIONALLY = 0x0000
	BRANCH_ON_FALSE        = 0x0200
	BRANCH_ON_TRUE         = 0x0400
)

const stackSize = 512

var BytecodeDataStack []*Data = make([]*Data, stackSize, stackSize)
var BytecodeStackPointer = 0

func ResetBytecodeRuntime() {
	BytecodeStackPointer = 0
}

func call(name *Data, arity uint, env *SymbolTableFrame) (result *Data, err error) {
	f := env.ValueOfWithFunctionSlotCheck(name, true)
	args := make([]*Data, 0, arity)
	for i := arity; i > 0; i -= 1 {
		args = append(args, pop())
	}
	argList := Reverse(ArrayToList(args))
	if PrimitiveP(f) {
		return PrimitiveValue(f).Apply(argList, env)
	} else if FunctionP(f) {
		return FunctionValue(f).Apply(argList, env)
	} else if CompiledFunctionP(f) {
		return CompiledFunctionValue(f).Apply(argList, env)
	} else {
		err = errors.New(fmt.Sprintf("Bytecode CALL instruction expected a function name, but was given %s.", String(f)))
		return
	}
}

func push(d *Data) {
	if BytecodeStackPointer == stackSize-1 {
		panic(errors.New("Bytecode runtime stack overflow."))
	}
	fmt.Printf("BYTECODE: pushing %s\n", String(d))
	BytecodeDataStack[BytecodeStackPointer] = d
	BytecodeStackPointer += 1
}

func pop() *Data {
	if BytecodeStackPointer == 0 {
		panic(errors.New("Bytecode runtime stack underflow."))
	}
	BytecodeStackPointer -= 1
	temp := BytecodeDataStack[BytecodeStackPointer]
	BytecodeDataStack[BytecodeStackPointer] = nil
	return temp
}

func getDataPointerAt(code []uint16, index int) (result *Data) {
	ptr := uintptr(uint64(code[index]) + (uint64(code[index+1]) << 16) + (uint64(code[index+2]) << 32) + (uint64(code[index+3]) << 48))
	return (*Data)(unsafe.Pointer(ptr))
}

func ExecuteBytecode(code []uint16, env *SymbolTableFrame) (result *Data, err error) {
	var programCounter int = 0

	for programCounter < len(code) {
		currentWord := code[programCounter]
		fmt.Printf("BYTECODE: fetched instruction word %04x: %04x\n", programCounter, currentWord)
		fmt.Printf("BYTECODE: executing %04x: %x\n", programCounter, uint(currentWord&OPCODE_MASK)>>12)
		switch currentWord & OPCODE_MASK {
		case OPCODE_CALL:
			arity := uint(currentWord&FUNCTION_ARITY_MASK) >> FUNCTION_ARITY_SHIFT
			data, e := call(getDataPointerAt(code, programCounter+1), arity, env)
			if e != nil {
				err = e
				return
			}
			push(data)
			programCounter += 5
		case OPCODE_CONSTANT:
			if (currentWord & CONSTANT_SIZE_MASK) == SHORT_CONSTANT {
				dataType := currentWord & SHORT_CONSTANT_TYPE_MASK
				value := currentWord & SHORT_CONSTANT_MASK
				if dataType == CONSTANT_INTEGER {
					push(IntegerWithValue(int64(value)))
				} else if dataType == CONSTANT_BOOLEAN {
					push(BooleanWithValue(value == 1))
				}
				programCounter += 1
			} else {
				push(getDataPointerAt(code, programCounter+1))
				programCounter += 5
			}
		case OPCODE_VAR_REF:
			sym := getDataPointerAt(code, programCounter+1)
			v, e := Eval(sym, env)
			if e != nil {
				err = e
				return
			}
			push(v)
			programCounter += 5
		case OPCODE_RETURN:
			result = pop()
			return
		case OPCODE_BRANCH:
			var delta int = 0
			if (currentWord & BRANCH_SIZE_MASK) == SHORT_BRANCH {
				delta = int(int16(int8(uint16(currentWord & BRANCH_SHORT_OFFSET_MASK))))
				programCounter += 1
			} else {
				delta = int(int16(code[programCounter+1]))
				programCounter += 2
			}

			condition := currentWord & BRANCH_CONDITION_MASK
			switch condition {
			case BRANCH_UNCONDITIONALLY:
				programCounter += delta
			case BRANCH_ON_FALSE:
				c := BooleanValue(pop())
				if !c {
					programCounter += delta
				}
			case BRANCH_ON_TRUE:
				c := BooleanValue(pop())
				if c {
					programCounter += delta
				}
			}
		default:
		}
	}
	return
}

func Dissassemble(code []uint16) {
	var pc int = 0
	for pc < len(code) {
		currentWord := code[pc]
		fmt.Printf("%04x: %04x ", pc, currentWord)
		switch currentWord & OPCODE_MASK {
		case OPCODE_CALL:
			arity := uint(currentWord&FUNCTION_ARITY_MASK) >> FUNCTION_ARITY_SHIFT
			fmt.Printf("CALL %s, %d\n", String(getDataPointerAt(code, pc+1)), arity)
			pc += 5
		case OPCODE_CONSTANT:
			fmt.Printf("CONSTANT")
			if (currentWord & CONSTANT_SIZE_MASK) == SHORT_CONSTANT {
				fmt.Printf("# ")
				dataType := currentWord & SHORT_CONSTANT_TYPE_MASK
				value := currentWord & SHORT_CONSTANT_MASK
				if dataType == CONSTANT_INTEGER {
					fmt.Printf("%d", value)
				} else if dataType == CONSTANT_BOOLEAN {
					if value == 1 {
						fmt.Printf("true")
					} else {
						fmt.Printf("false")
					}
				}
				pc += 1
			} else {
				fmt.Printf(" %s", String(getDataPointerAt(code, pc+1)))
				pc += 5
			}
			fmt.Printf("\n")
		case OPCODE_VAR_REF:
			sym := getDataPointerAt(code, pc+1)
			fmt.Printf("VARREF %s\n", StringValue(sym))
			pc += 5
		case OPCODE_RETURN:
			fmt.Printf("RETURN\n")
			pc += 1
		case OPCODE_BRANCH:
			fmt.Printf("BRA")

			var delta int = 0
			if (currentWord & BRANCH_SIZE_MASK) == SHORT_BRANCH {
				fmt.Printf("S")
				delta = int(int16(int8(uint16(currentWord & BRANCH_SHORT_OFFSET_MASK))))
				pc += 1
			} else {
				fmt.Printf("L")
				delta = int(int16(code[pc+1]))
				pc += 2
			}

			condition := currentWord & BRANCH_CONDITION_MASK
			switch condition {
			case BRANCH_UNCONDITIONALLY:
			case BRANCH_ON_FALSE:
				fmt.Printf("F ")
			case BRANCH_ON_TRUE:
				fmt.Printf("T ")
			}
			fmt.Printf("%d\n", delta)
		default:
		}
	}
}
