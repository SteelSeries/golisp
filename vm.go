// Copyright 2017 Dave Astels.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the byte code VM.

package golisp

import (
	"errors"
	"fmt"
	"github.com/golang/glog"
	"strings"
	"unsafe"
)

func RegisterVMPrimitives() {
	MakePrimitiveFunction("execute", "1", machineImpl)
}

const (
	ZERO_CONST = iota
	ONE_CONST
	TWO_CONST
	TRUE_CONST
	FALSE_CONST
	NIL_CONST
	LVAR
	LSET
	GVAR
	GSET
	POP
	CONST
	JUMP
	FJUMP
	TJUMP
	SAVE
	RETURN
	CALLJ
	ARGS
	ARGSDOT
	FN
	PRIM
	SET_CC
	CC
	CAR
	CDR
	CADR
	NOT
	NILP
	LIST1
	RANDOM
	PLUS
	MINUS
	TIMES
	DIVIDE
	LT
	GT
	LTEQ
	GTEQ
	CONS
	LIST2
	EQV
	NEQV
	EQ
	NEQ
	EQUAL
	NEQUAL
	LIST3
	NAMEBANG
	HALT
)

var opcodeNames = [HALT + 1]string{
	"ZERO_CONST",
	"ONE_CONST",
	"TWO_CONST",
	"TRUE_CONST",
	"FALSE_CONST",
	"NIL_CONST",
	"LVAR",
	"LSET",
	"GVAR",
	"GSET",
	"POP",
	"CONST",
	"JUMP",
	"FJUMP",
	"TJUMP",
	"SAVE",
	"RETURN",
	"CALLJ",
	"ARGS",
	"ARGSDOT",
	"FN",
	"PRIM",
	"SET_CC",
	"CC",
	"CAR",
	"CDR",
	"CADR",
	"NOT",
	"NILP",
	"LIST1",
	"RANDOM",
	"PLUS",
	"MINUS",
	"TIMES",
	"DIVIDE",
	"LT",
	"GT",
	"LTEQ",
	"GTEQ",
	"CONS",
	"LIST2",
	"EQV",
	"NEQV",
	"EQ",
	"NEQ",
	"EQUAL",
	"NEQUAL",
	"LIST3",
	"NAMEBANG",
	"HALT",
}

var ZeroConstant *Data = IntegerWithValue(0)
var OneConstant *Data = IntegerWithValue(1)
var TwoConstant *Data = IntegerWithValue(2)

//------------------------------------------------------------------------------
// Stack

var MAXSTACK = 16 * 1024

type RuntimeStack struct {
	storage []*Data
	sp      int
}

func newRuntimeStack() *RuntimeStack {
	return &RuntimeStack{storage: make([]*Data, MAXSTACK), sp: 0}
}

func (self *RuntimeStack) Push(value *Data) (err error) {
	if self.sp >= MAXSTACK {
		return errors.New("Stack overflow")
	}
	self.storage[self.sp] = value
	self.sp++
	return nil
}

func (self *RuntimeStack) Pop() (value *Data, err error) {
	if self.sp == 0 {
		return nil, errors.New("Stack underflow")
	}
	self.sp--
	return self.storage[self.sp], nil
}

func (self *RuntimeStack) Top() (value *Data, err error) {
	if self.sp == 0 {
		return nil, errors.New("Stack empty")
	}
	return self.storage[self.sp-1], nil
}

func (self *RuntimeStack) Dump() {
	glog.Info("Stack: ")
	if self.sp == 0 {
		glog.Info("empty")
	} else {
		for i := 0; i < self.sp; i++ {
			if i > 0 {
				glog.Info("       ")
			}
			glog.Infof("%s", String(self.storage[i]))
		}
	}

}

//------------------------------------------------------------------------------
// Local environment management

type LocalEnvFrame struct {
	Values []*Data
	Next   *LocalEnvFrame
}

func newEnv(size int, parent *LocalEnvFrame) *LocalEnvFrame {
	return &LocalEnvFrame{Values: make([]*Data, size), Next: parent}
}

func newEnvFrom(savedEnv *Data) *LocalEnvFrame {
	frames := ToArray(savedEnv)
	var newFrame *LocalEnvFrame = nil
	for i := len(frames) - 1; i >= 0; i-- {
		newFrame = &LocalEnvFrame{Values: ToArray(frames[i]), Next: newFrame}
	}
	return newFrame
}

func (self *LocalEnvFrame) Package() *Data {
	frames := make([]*Data, 0, 16)
	for f := self; f != nil; f = f.Next {
		frames = append(frames, ArrayToList(f.Values))
	}
	glog.Infof("packaged env: %s", String(ArrayToList(frames)))
	return ArrayToList(frames)
}

func (self *LocalEnvFrame) Get(frameIndex int64, varIndex int64) (result *Data, err error) {
	env := self
	for i := frameIndex; i > 0; i-- {
		env = env.Next
		if env == nil {
			return nil, errors.New("local environment get: Local frame depth out of range")
		}
	}

	if varIndex < 0 || varIndex >= int64(len(env.Values)) {
		return nil, errors.New(fmt.Sprintf("local environment get: Local variable index out of range: %d", varIndex))
	}

	return env.Values[varIndex], nil
}

func (self *LocalEnvFrame) Set(frameIndex int64, varIndex int64, value *Data) (err error) {
	env := self
	for i := frameIndex; i > 0; i-- {
		env = env.Next
		if env == nil {
			return errors.New("local environment set: Local frame depth out of range")
		}
	}

	if varIndex < 0 || varIndex >= int64(len(env.Values)) {
		return errors.New(fmt.Sprintf("local environment set: Local variable index out of range: %d", varIndex))
	}

	env.Values[varIndex] = value
	return nil
}

func (self *LocalEnvFrame) Dump() {
	glog.Info("Local environment:")
	for frameIndex, env := 0, self; env != nil; frameIndex, env = frameIndex+1, env.Next {
		glog.Infof("  Frame %2d: ", frameIndex)
		if len(env.Values) == 0 {
			glog.Info("\n")
		} else {
			for offset := 0; offset < len(env.Values); offset++ {
				if offset > 0 {
					glog.Info("            ")
				}
				glog.Infof("%2d: %s\n", offset, String(env.Values[offset]))
			}
		}
	}
}

type CompiledFunctionInvocation struct {
	code *Data
	env  *LocalEnvFrame
}

//------------------------------------------------------------------------------
// Global variable management

func getGvar(name *Data) (result *Data, err error) {
	return Global.ValueOf(name), nil
}

func setGvar(name *Data, value *Data) (err error) {
	Global.BindTo(name, value)
	return nil
}

//------------------------------------------------------------------------------
// Return address

type ReturnAddress struct {
	pc  int
	f   *Data
	env *LocalEnvFrame
}

func makeReturnAddr(pc int, f *Data, env *LocalEnvFrame) (result *Data, err error) {
	if pc >= Length(FrameValue(f).Get("code:")) {
		return nil, errors.New("Return address out of range")
	}
	return ObjectWithTypeAndValue("returnAddress", unsafe.Pointer(&ReturnAddress{pc: pc, f: f, env: env})), nil
}

//------------------------------------------------------------------------------
// Bytecode Executer

var CompiledFunctionStack *RuntimeStack

func machineImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return executeBytecode(First(args), env)
}

func humanifyInstruction(code *Data) string {
	bytecode := VectorValue(code)
	opInt := int(IntegerValue(bytecode[0]))
	if opInt > HALT {
		return fmt.Sprintf("UNKNOWN OPCODE: %d", bytecode[0])
	}
	opcode := opcodeNames[opInt]
	if len(bytecode) == 1 {
		return opcode
	}
	parts := make([]string, len(bytecode))
	parts[0] = opcode
	for i := 1; i < len(bytecode); i++ {
		parts[i] = String(bytecode[i])
	}
	return strings.Join(parts, " ")
}

func executeBytecode(f *Data, env *SymbolTableFrame) (result *Data, err error) {
	fmt.Printf("Execute: %s\n", String(f))
	CompiledFunctionStack = newRuntimeStack()
	fMap := FrameValue(f)
	var code = VectorValue(fMap.Get("code:"))
	var pc = 0
	var localEnv *LocalEnvFrame = newEnv(0, nil)
	var nArgs = 0
	var val *Data = nil
	var val2 *Data = nil
	var val3 *Data = nil
	var mathResult float64 = 0.0
	glog.Infof("Entering VM: %s\n", String(f.Code))
	for {
		instr := VectorValue(code[pc])

		// print the execution context
		glog.Info("//----------------------------------------")
		CompiledFunctionStack.Dump()
		localEnv.Dump()
		glog.Infof("Executing at %3d: %s  ; %s\n", pc, String(code[pc]), humanifyInstruction(code[pc]))

		pc++
		opcode := IntegerValue(instr[0])
		switch opcode {
		//Constants
		case ZERO_CONST:
			err = CompiledFunctionStack.Push(ZeroConstant)
			if err != nil {
				return
			}
		case ONE_CONST:
			err = CompiledFunctionStack.Push(OneConstant)
			if err != nil {
				return
			}
		case TWO_CONST:
			err = CompiledFunctionStack.Push(TwoConstant)
			if err != nil {
				return
			}
		case TRUE_CONST:
			err = CompiledFunctionStack.Push(LispTrue)
			if err != nil {
				return
			}
		case FALSE_CONST:
			err = CompiledFunctionStack.Push(LispFalse)
			if err != nil {
				return
			}
		case NIL_CONST:
			err = CompiledFunctionStack.Push(EmptyCons())
			if err != nil {
				return
			}

			// Variable/stack manipulation instructions
		case LVAR:
			val, err = localEnv.Get(IntegerValue(instr[1]), IntegerValue(instr[2]))
			if err != nil {
				return
			}
			err = CompiledFunctionStack.Push(val)
			if err != nil {
				return
			}
		case LSET:
			val, err = CompiledFunctionStack.Top()
			if err != nil {
				return
			}
			err = localEnv.Set(IntegerValue(instr[1]), IntegerValue(instr[2]), val)
			if err != nil {
				return
			}
		case GVAR:
			val, err = getGvar(instr[1])
			if err != nil {
				return
			}
			err = CompiledFunctionStack.Push(val)
			if err != nil {
				return
			}
		case GSET:
			val, err = CompiledFunctionStack.Top()
			if err != nil {
				return
			}
			err = setGvar(instr[1], val)
			if err != nil {
				return
			}
		case POP:
			_, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}
		case CONST:
			val = instr[1]
			err = CompiledFunctionStack.Push(val)
			if err != nil {
				return
			}

			// Branching instructions
		case JUMP:
			if int(IntegerValue(instr[1])) >= len(code) {
				return nil, errors.New("JUMP target is out of range")
			}
			pc = int(IntegerValue(instr[1]))
		case FJUMP:
			if int(IntegerValue(instr[1])) >= len(code) {
				return nil, errors.New("FJUMP target is out of range")
			}
			val, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}
			if !BooleanValue(val) {
				pc = int(IntegerValue(instr[1]))
			}
		case TJUMP:
			if int(IntegerValue(instr[1])) >= len(code) {
				return nil, errors.New("TJUMP target is out of range")
			}
			val, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}
			if BooleanValue(val) {
				pc = int(IntegerValue(instr[1]))
			}

			// Function call/return instructions
		case SAVE:
			if int(IntegerValue(instr[1])) >= len(code) {
				return nil, errors.New("SAVE target is out of range")
			}
			val, err = makeReturnAddr(int(IntegerValue(instr[1])), f, localEnv)
			if err != nil {
				return
			}
			err = CompiledFunctionStack.Push(val)
			if err != nil {
				return
			}
		case RETURN: // return value is top of stack, return address is second
			val, err = CompiledFunctionStack.Pop() // save the return value
			if err != nil {
				return
			}

			var retAddrObj *Data
			retAddrObj, err = CompiledFunctionStack.Pop()
			if err != nil {
				return val, nil
			}
			if ObjectType(retAddrObj) != "returnAddress" {
				return nil, errors.New("Bad return type object")
			}

			retAddr := (*ReturnAddress)(ObjectValue(retAddrObj))
			f = retAddr.f
			fMap = FrameValue(f)
			code = VectorValue(fMap.Get("code:"))
			localEnv = retAddr.env
			pc = retAddr.pc

			err = CompiledFunctionStack.Push(val) // put the return value back
			if err != nil {
				return
			}
		case CALLJ:
			f, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}
			nArgs = int(IntegerValue(instr[1]))
			if FrameP(f) {
				localEnv = localEnv.Next // discard the top frame
				fMap = FrameValue(f)
				fname := fMap.Get("name:")
				if NilP(fname) {
					fname = StringWithValue("<anon>")
				}
				fmt.Printf("Calling compiled function: %s\n", (String(fname)))
				code = VectorValue(fMap.Get("code:"))
				localEnv = newEnvFrom(fMap.Get("env:"))
				pc = 0
			} else {
				err = errors.New(fmt.Sprintf("Compiled function expected for CALLJ, but got %s", String(f)))
				return
			}
		case ARGS:
			if nArgs != int(IntegerValue(instr[1])) {
				return nil, errors.New(fmt.Sprintf("Wrong number of arguments to %s: %d expected, %d supplied", fMap.Get("name:"), int(IntegerValue(instr[1])), nArgs))
			}
			localEnv = newEnv(int(IntegerValue(instr[1])), localEnv)
			for i := nArgs - 1; i >= 0; i-- {
				val, err = CompiledFunctionStack.Pop()
				if err != nil {
					return
				}
				localEnv.Values[i] = val
			}
		case ARGSDOT:
			if nArgs >= Length(instr[1]) {
				return nil, errors.New(fmt.Sprintf("Wrong number of arguments to %s: %d or more expected, %d supplied", fMap.Get("name:"), instr[1], nArgs))
			}
			argValue := int(IntegerValue(instr[1]))
			localEnv = newEnv(argValue+1, localEnv)
			restArgs := make([]*Data, 0, nArgs-argValue)
			for i := nArgs - argValue; i > 0; i-- {
				val, err = CompiledFunctionStack.Pop()
				if err != nil {
					return
				}
				restArgs = append(restArgs, val)
			}
			for i := argValue - 1; i >= 0; i-- {
				val, err = CompiledFunctionStack.Pop()
				if err != nil {
					return
				}
				localEnv.Values[i] = val
			}
		case FN:
			var fn FrameMap = make(map[string]*Data, 2)
			fn.Set("code:", FrameValue(instr[1]).Get("code:"))
			fn.Set("env:", localEnv.Package())
			err = CompiledFunctionStack.Push(FrameWithValue(&fn))
			if err != nil {
				return
			}
		case PRIM:
			if !SymbolP(instr[1]) {
				return nil, errors.New(fmt.Sprintf("PRIM requires a primitive function name, but got %s", String(instr[1])))
			}
			prim := env.ValueOf(instr[1])
			if !PrimitiveP(prim) {
				return nil, errors.New(fmt.Sprintf("PRIM requires a primitive function, but got %s", String(prim)))
			}

			// process the args
			if !IntegerP(instr[2]) {
				return nil, errors.New(fmt.Sprintf("PRIM requires an integer argument count, but got %s", String(instr[2])))
			}
			nArgs = int(IntegerValue(instr[2]))
			args := make([]*Data, nArgs)
			for i := nArgs - 1; i >= 0; i-- {
				args[i], err = CompiledFunctionStack.Pop()
				if err != nil {
					return
				}
			}

			// eval the primitive
			val, err = PrimitiveValue(prim).ApplyWithoutEval(ArrayToList(args), Global)
			glog.Infof("Calling primitive function: %s\n", String(instr[1]))
			if err != nil {
				return
			}

			// put the result onto the stack
			err = CompiledFunctionStack.Push(val)
			if err != nil {
				return
			}

			// Continuation instructions
		case SET_CC:
			val, err = CompiledFunctionStack.Top()
			if err != nil {
				return
			}
			if !ObjectP(val) {
				return nil, errors.New(fmt.Sprintf("SET_CC expected a stack object, but received a %s", TypeName(TypeOf(val))))
			}
			if ObjectType(val) != "stack" {
				return nil, errors.New(fmt.Sprintf("SET_CC expected a stack object, but received a %s", ObjectType(val)))
			}
			CompiledFunctionStack = (*RuntimeStack)(ObjectValue(val))
		case CC:

			// Unary operations
		case CAR:
			val, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}

			err = CompiledFunctionStack.Push(Car(val))
			if err != nil {
				return
			}
		case CDR:
			val, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}

			err = CompiledFunctionStack.Push(Cdr(val))
			if err != nil {
				return
			}
		case CADR:
			val, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}

			err = CompiledFunctionStack.Push(Cadr(val))
			if err != nil {
				return
			}
		case NOT:
			val, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}

			err = CompiledFunctionStack.Push(BooleanWithValue(!BooleanValue(val)))
			if err != nil {
				return
			}
		case NILP:
			val, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}

			err = CompiledFunctionStack.Push(BooleanWithValue(NilP(val)))
			if err != nil {
				return
			}
		case LIST1:
			val, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}

			err = CompiledFunctionStack.Push(InternalMakeList(val))
			if err != nil {
				return
			}

			// Binary operations
		case PLUS:
			isFloat := false

			val2, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}
			val, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}
			isFloat = FloatP(val) || FloatP(val2)
			mathResult = FloatValue(val) + FloatValue(val2)
			if isFloat {
				err = CompiledFunctionStack.Push(FloatWithValue(mathResult))
			} else {
				err = CompiledFunctionStack.Push(IntegerWithValue(int64(mathResult)))
			}
			if err != nil {
				return
			}
		case MINUS:
			isFloat := false

			val2, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}
			val, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}
			isFloat = FloatP(val) || FloatP(val2)
			mathResult = FloatValue(val) - FloatValue(val2)
			if isFloat {
				err = CompiledFunctionStack.Push(FloatWithValue(mathResult))
			} else {
				err = CompiledFunctionStack.Push(IntegerWithValue(int64(mathResult)))
			}
			if err != nil {
				return
			}
		case TIMES:
			isFloat := false

			val2, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}
			val, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}
			isFloat = FloatP(val) || FloatP(val2)
			mathResult = FloatValue(val) * FloatValue(val2)
			if isFloat {
				err = CompiledFunctionStack.Push(FloatWithValue(mathResult))
			} else {
				err = CompiledFunctionStack.Push(IntegerWithValue(int64(mathResult)))
			}
			if err != nil {
				return
			}
		case DIVIDE:
			isFloat := false

			val2, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}
			val, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}
			isFloat = FloatP(val) || FloatP(val2)
			mathResult = FloatValue(val) / FloatValue(val2)
			if isFloat {
				err = CompiledFunctionStack.Push(FloatWithValue(mathResult))
			} else {
				err = CompiledFunctionStack.Push(IntegerWithValue(int64(mathResult)))
			}
			if err != nil {
				return
			}
		case LT:
			val2, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}
			val, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}

			err = CompiledFunctionStack.Push(BooleanWithValue(FloatValue(val) < FloatValue(val2)))
			if err != nil {
				return
			}
		case GT:
			val2, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}
			val, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}

			err = CompiledFunctionStack.Push(BooleanWithValue(FloatValue(val) > FloatValue(val2)))
			if err != nil {
				return
			}
		case LTEQ:
			val2, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}
			val, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}

			err = CompiledFunctionStack.Push(BooleanWithValue(FloatValue(val) <= FloatValue(val2)))
			if err != nil {
				return
			}
		case GTEQ:
			val2, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}
			val, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}

			err = CompiledFunctionStack.Push(BooleanWithValue(FloatValue(val) >= FloatValue(val2)))
			if err != nil {
				return
			}
		case CONS:
			val2, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}
			val, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}
			err = CompiledFunctionStack.Push(Cons(val, val2))
			if err != nil {
				return
			}
		case LIST2:
			val2, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}
			val, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}
			err = CompiledFunctionStack.Push(InternalMakeList(val, val2))
			if err != nil {
				return
			}
		case EQV:
			val2, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}
			val, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}
			err = CompiledFunctionStack.Push(BooleanWithValue(IsEqv(val, val2)))
			if err != nil {
				return
			}
		case NEQV:
			val2, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}
			val, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}
			err = CompiledFunctionStack.Push(BooleanWithValue(!IsEqv(val, val2)))
			if err != nil {
				return
			}
		case EQ:
			val2, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}
			val, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}
			err = CompiledFunctionStack.Push(BooleanWithValue(IsEq(val, val2)))
			if err != nil {
				return
			}
		case NEQ:
			val2, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}
			val, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}
			err = CompiledFunctionStack.Push(BooleanWithValue(!IsEq(val, val2)))
			if err != nil {
				return
			}
		case EQUAL:
			val2, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}
			val, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}
			err = CompiledFunctionStack.Push(BooleanWithValue(IsEqual(val, val2)))
			if err != nil {
				return
			}
		case NEQUAL:
			val2, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}
			val, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}
			err = CompiledFunctionStack.Push(BooleanWithValue(!IsEqual(val, val2)))
			if err != nil {
				return
			}
		case LIST3:
			val3, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}
			val2, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}
			val, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}
			err = CompiledFunctionStack.Push(InternalMakeList(val, val2, val3))
			if err != nil {
				return
			}
		case NAMEBANG:
			// pop the name
			val, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}
			if !StringP(val) && !SymbolP(val) {
				err = errors.New("Stringy value expected for NAMEBANG name")
			}

			// pop the function
			val2, err = CompiledFunctionStack.Pop()
			if err != nil {
				return
			}
			if !FrameP(val2) {
				err = errors.New("Function expected for NAMEBANG function")
				return
			}

			fMap = FrameValue(val2)
			fMap.Set("name:", val)
			CompiledFunctionStack.Push(val2)
		case HALT:
			return CompiledFunctionStack.Top()
		}
	}
}
