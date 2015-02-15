// Copyright 2015 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file implements the bytecode machine.

package golisp

import (
	"errors"
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
	FUNCTION_INDEX_MASK = 0x03FF
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

var FunctionTable []*Data = make([]*Data, 0, 128)

var programCounter int

const stackSize = 512

var BytecodeDataStack []*Data = make([]*Data, stackSize, stackSize)
var BytecodeStackPointer = 0

func call(index uint) (err error) {
	//	f := FunctionTable[index]
	return nil
}

func push(d *Data) {
	if BytecodeStackPointer == stackSize-1 {
		panic(errors.New("Bytecode runtime stack overflow."))
	}
	BytecodeDataStack[BytecodeStackPointer] = d
	BytecodeStackPointer += 1
}

func pop() (result *Data) {
	if BytecodeStackPointer == 0 {
		panic(errors.New("Bytecode runtime stack underflow."))
	}
	BytecodeStackPointer -= 1
	temp := BytecodeDataStack[BytecodeStackPointer]
	BytecodeDataStack[BytecodeStackPointer] = nil
	return temp
}

func getDataPointerAt(index int) (result *Data) {
	return
}

func ExecuteBytecode(code []uint16, env *SymbolTableFrame) (result *Data, err error) {
	programCounter = 0

	for programCounter < len(code) {
		currentWord := code[programCounter]
		switch currentWord & OPCODE_MASK {
		case OPCODE_CALL:
			index := uint(currentWord & FUNCTION_INDEX_MASK)
			call(index)
			programCounter += 1
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
				push(getDataPointerAt(programCounter + 1))
				programCounter += 5
			}
		case OPCODE_VAR_REF:
			sym := getDataPointerAt(programCounter + 1)
			v, e := Eval(sym, env)
			if e != nil {
				err = e
				return
			}
			push(v)
		case OPCODE_RETURN:
			result = pop()
			return
		case OPCODE_BRANCH:
			var delta int = 0
			condition := currentWord & BRANCH_CONDITION_MASK
			if (currentWord & BRANCH_SIZE_MASK) == SHORT_BRANCH {
				delta = int(int16(int8(uint16(currentWord & BRANCH_SHORT_OFFSET_MASK))))
				programCounter += 1
			} else {
				delta = int(int16(code[programCounter+1]))
				programCounter += 2
			}

			condition = currentWord & BRANCH_CONDITION_MASK
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
