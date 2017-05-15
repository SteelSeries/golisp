// Copyright 2017 Dave Astels.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the byte code VM.

package golisp

import (
	"fmt"
	"strings"
	"unsafe"
)

func RegisterVMPrimitives() {
	MakeTypedPrimitiveFunction("execute", "1", machineImpl, []uint32{CompiledFunctionType})
}

func machineImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return ExecuteBytecode(CompiledFunctionValue(First(args)), env)
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
	FUNC
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
	"FUNC",
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

func (self *RuntimeStack) Push(value *Data) {
	if self.sp >= MAXSTACK {
		panic("Push: Stack overflow")
	}
	self.storage[self.sp] = value
	self.sp++
}

func (self *RuntimeStack) Pop() *Data {
	if self.sp == 0 {
		panic("Pop: Stack underflow")
	}
	self.sp--
	return self.storage[self.sp]
}

func (self *RuntimeStack) Top() *Data {
	if self.sp == 0 {
		panic("Top: Stack underflow")
	}
	return self.storage[self.sp-1]
}

func (self *RuntimeStack) Empty() bool {
	return self.sp == 0
}

func (self *RuntimeStack) Dump() {
	fmt.Printf("Stack: ")
	if self.sp == 0 {
		fmt.Printf("empty")
	} else {
		for i := 0; i < self.sp; i++ {
			if i > 0 {
				fmt.Printf("       ")
			}
			fmt.Printf("%s", String(self.storage[i]))
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
	// fmt.Printf("packaged env: %s", String(ArrayToList(frames)))
	return ArrayToList(frames)
}

func (self *LocalEnvFrame) Get(frameIndex int64, varIndex int64) *Data {
	env := self
	for i := frameIndex; i > 0; i-- {
		env = env.Next
	}
	return env.Values[varIndex]
}

func (self *LocalEnvFrame) Set(frameIndex int64, varIndex int64, value *Data) {
	env := self
	for i := frameIndex; i > 0; i-- {
		env = env.Next
	}
	env.Values[varIndex] = value
}

func (self *LocalEnvFrame) Dump() {
	fmt.Printf("Local environment:")
	for frameIndex, env := 0, self; env != nil; frameIndex, env = frameIndex+1, env.Next {
		fmt.Printf("  Frame %2d: ", frameIndex)
		if len(env.Values) == 0 {
			fmt.Printf("\n")
		} else {
			for offset := 0; offset < len(env.Values); offset++ {
				if offset > 0 {
					fmt.Printf("            ")
				}
				fmt.Printf("%2d: %s\n", offset, String(env.Values[offset]))
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

func getGvar(name *Data) (result *Data) {
	return Global.ValueOf(name)
}

func setGvar(name *Data, value *Data) {
	Global.BindTo(name, value)
}

//------------------------------------------------------------------------------
// Return address

type ReturnAddress struct {
	pc  int
	f   *CompiledFunction
	env *LocalEnvFrame
}

func makeReturnAddr(pc int, f *CompiledFunction, env *LocalEnvFrame) *Data {
	return ObjectWithTypeAndValue("returnAddress", unsafe.Pointer(&ReturnAddress{pc: pc, f: f, env: env}))
}

//------------------------------------------------------------------------------
// Bytecode Executer

var CompiledFunctionStack *RuntimeStack

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

func ExecuteBytecode(f *CompiledFunction, env *SymbolTableFrame) (result *Data, err error) {
	return ExecuteBytecodeWithArgs(f, make([]*Data, 0), env)
}

func ExecuteBytecodeWithArgs(f *CompiledFunction, args []*Data, env *SymbolTableFrame) (result *Data, err error) {
	// fmt.Printf("Execute: %s\n", f.String())
	CompiledFunctionStack = newRuntimeStack()
	for _, arg := range args {
		CompiledFunctionStack.Push(arg)
	}
	var nArgs = len(args)

	var code = VectorValue(f.Code)
	var pc = 0
	var localEnv *LocalEnvFrame = newEnv(0, nil)
	var val *Data = nil
	var val2 *Data = nil
	var val3 *Data = nil
	var mathResult float64 = 0.0
	// fmt.Printf("Entering VM: %s\n", String(f.Code))
	for {
		instr := VectorValue(code[pc])

		// print the execution context
		// fmt.Printf("//----------------------------------------\n")
		// CompiledFunctionStack.Dump()
		// localEnv.Dump()
		// fmt.Printf("Executing at %3d: %s  ; %s\n", pc, String(code[pc]), humanifyInstruction(code[pc]))

		pc++
		opcode := IntegerValue(instr[0])
		switch opcode {
		//Constants
		case ZERO_CONST:
			CompiledFunctionStack.Push(ZeroConstant)
		case ONE_CONST:
			CompiledFunctionStack.Push(OneConstant)
		case TWO_CONST:
			CompiledFunctionStack.Push(TwoConstant)
		case TRUE_CONST:
			CompiledFunctionStack.Push(LispTrue)
		case FALSE_CONST:
			CompiledFunctionStack.Push(LispFalse)
		case NIL_CONST:
			CompiledFunctionStack.Push(EmptyCons())

			// Variable/stack manipulation instructions
		case LVAR:
			val = localEnv.Get(IntegerValue(instr[1]), IntegerValue(instr[2]))
			CompiledFunctionStack.Push(val)
		case LSET:
			val = CompiledFunctionStack.Top()
			localEnv.Set(IntegerValue(instr[1]), IntegerValue(instr[2]), val)
		case GVAR:
			val = getGvar(instr[1])
			CompiledFunctionStack.Push(val)
		case GSET:
			val = CompiledFunctionStack.Top()
			setGvar(instr[1], val)
		case POP:
			CompiledFunctionStack.Pop()
		case CONST:
			val = instr[1]
			CompiledFunctionStack.Push(val)

			// Branching instructions
		case JUMP:
			pc = int(IntegerValue(instr[1]))
		case FJUMP:
			if CompiledFunctionStack.Pop() == LispFalse {
				pc = int(IntegerValue(instr[1]))
			}
		case TJUMP:
			if CompiledFunctionStack.Pop() != LispFalse {
				pc = int(IntegerValue(instr[1]))
			}

			// Function call/return instructions
		case SAVE:
			val = makeReturnAddr(int(IntegerValue(instr[1])), f, localEnv)
			CompiledFunctionStack.Push(val)
		case RETURN: // return value is top of stack, return address is second
			val = CompiledFunctionStack.Pop() // save the return value
			// if the stack is empty, return the value from the VM execution
			if CompiledFunctionStack.Empty() {
				return val, nil
			}
			// otherwise handle the return address
			var retAddrObj *Data
			retAddrObj = CompiledFunctionStack.Pop()
			retAddr := (*ReturnAddress)(ObjectValue(retAddrObj))
			f = retAddr.f
			code = VectorValue(f.Code)
			localEnv = retAddr.env
			pc = retAddr.pc

			CompiledFunctionStack.Push(val) // put the return value back
		case CALLJ:
			var fObj *Data
			fObj = CompiledFunctionStack.Pop()
			f = CompiledFunctionValue(fObj)
			nArgs = int(IntegerValue(instr[1]))
			localEnv = localEnv.Next // discard the top frame
			fname := f.Name
			if fname == "" {
				fname = "<anon>"
			}
			// fmt.Printf("Calling compiled function: %s\n", fname)
			code = VectorValue(f.Code)
			localEnv = newEnvFrom(f.Env)
			pc = 0
		case ARGS:
			localEnv = newEnv(int(IntegerValue(instr[1])), localEnv)
			for i := nArgs - 1; i >= 0; i-- {
				val = CompiledFunctionStack.Pop()
				localEnv.Values[i] = val
			}
		case ARGSDOT:
			argValue := int(IntegerValue(instr[1]))
			localEnv = newEnv(argValue+1, localEnv)
			restArgs := make([]*Data, 0, nArgs-argValue)
			for i := nArgs - argValue; i > 0; i-- {
				val = CompiledFunctionStack.Pop()
				restArgs = append(restArgs, val)
			}
			for i := argValue - 1; i >= 0; i-- {
				val = CompiledFunctionStack.Pop()
				localEnv.Values[i] = val
			}
			localEnv.Values[argValue-1] = ArrayToList(restArgs)
		case FN:
			fn := instr[1]
			CompiledFunctionValue(fn).Env = localEnv.Package()
			CompiledFunctionStack.Push(fn)
		case PRIM:
			prim := env.ValueOf(instr[1])
			// process the args
			nArgs = int(IntegerValue(instr[2]))
			args := make([]*Data, nArgs)
			for i := nArgs - 1; i >= 0; i-- {
				args[i] = CompiledFunctionStack.Pop()
			}

			// eval the primitive
			// fmt.Printf("Calling primitive function: %s\n", String(instr[1]))
			val, err = PrimitiveValue(prim).ApplyWithoutEval(ArrayToList(args), env)
			if err != nil {
				return
			}

			// put the result onto the stack
			CompiledFunctionStack.Push(val)

		case FUNC:
			fun := env.ValueOf(instr[1])
			// process the args
			nArgs = int(IntegerValue(instr[2]))
			args := make([]*Data, nArgs)
			for i := nArgs - 1; i >= 0; i-- {
				args[i] = CompiledFunctionStack.Pop()
			}

			// eval the function
			// fmt.Printf("Calling interpreted function: %s\n", String(instr[1]))
			val, err = FunctionValue(fun).ApplyWithoutEval(ArrayToList(args), env)
			if err != nil {
				return
			}

			// put the result onto the stack
			CompiledFunctionStack.Push(val)

		// Continuation instructions
		case SET_CC:
			val = CompiledFunctionStack.Top()
			CompiledFunctionStack = (*RuntimeStack)(ObjectValue(val))
		case CC:

			// Unary operations
		case CAR:
			val = CompiledFunctionStack.Pop()
			CompiledFunctionStack.Push(Car(val))
		case CDR:
			val = CompiledFunctionStack.Pop()
			CompiledFunctionStack.Push(Cdr(val))
		case CADR:
			val = CompiledFunctionStack.Pop()
			CompiledFunctionStack.Push(Cadr(val))
		case NOT:
			if CompiledFunctionStack.Pop() == LispFalse {
				val = LispTrue
			} else {
				val = LispFalse
			}
			CompiledFunctionStack.Push(val)
		case NILP:
			val = CompiledFunctionStack.Pop()
			CompiledFunctionStack.Push(BooleanWithValue(NilP(val)))
		case LIST1:
			val = CompiledFunctionStack.Pop()
			CompiledFunctionStack.Push(InternalMakeList(val))

			// Binary operations
		case PLUS:
			isFloat := false

			val2 = CompiledFunctionStack.Pop()
			val = CompiledFunctionStack.Pop()
			isFloat = FloatP(val) || FloatP(val2)
			mathResult = FloatValue(val) + FloatValue(val2)
			if isFloat {
				CompiledFunctionStack.Push(FloatWithValue(mathResult))
			} else {
				CompiledFunctionStack.Push(IntegerWithValue(int64(mathResult)))
			}
		case MINUS:
			isFloat := false

			val2 = CompiledFunctionStack.Pop()
			val = CompiledFunctionStack.Pop()
			isFloat = FloatP(val) || FloatP(val2)
			mathResult = FloatValue(val) - FloatValue(val2)
			if isFloat {
				CompiledFunctionStack.Push(FloatWithValue(mathResult))
			} else {
				CompiledFunctionStack.Push(IntegerWithValue(int64(mathResult)))
			}
		case TIMES:
			isFloat := false

			val2 = CompiledFunctionStack.Pop()
			val = CompiledFunctionStack.Pop()
			isFloat = FloatP(val) || FloatP(val2)
			mathResult = FloatValue(val) * FloatValue(val2)
			if isFloat {
				CompiledFunctionStack.Push(FloatWithValue(mathResult))
			} else {
				CompiledFunctionStack.Push(IntegerWithValue(int64(mathResult)))
			}
		case DIVIDE:
			isFloat := false

			val2 = CompiledFunctionStack.Pop()
			val = CompiledFunctionStack.Pop()
			isFloat = FloatP(val) || FloatP(val2)
			mathResult = FloatValue(val) / FloatValue(val2)
			if isFloat {
				CompiledFunctionStack.Push(FloatWithValue(mathResult))
			} else {
				CompiledFunctionStack.Push(IntegerWithValue(int64(mathResult)))
			}
		case LT:
			val2 = CompiledFunctionStack.Pop()
			val = CompiledFunctionStack.Pop()
			CompiledFunctionStack.Push(BooleanWithValue(FloatValue(val) < FloatValue(val2)))
		case GT:
			val2 = CompiledFunctionStack.Pop()
			val = CompiledFunctionStack.Pop()
			CompiledFunctionStack.Push(BooleanWithValue(FloatValue(val) > FloatValue(val2)))
		case LTEQ:
			val2 = CompiledFunctionStack.Pop()
			val = CompiledFunctionStack.Pop()
			CompiledFunctionStack.Push(BooleanWithValue(FloatValue(val) <= FloatValue(val2)))
		case GTEQ:
			val2 = CompiledFunctionStack.Pop()
			val = CompiledFunctionStack.Pop()
			CompiledFunctionStack.Push(BooleanWithValue(FloatValue(val) >= FloatValue(val2)))
		case CONS:
			val2 = CompiledFunctionStack.Pop()
			val = CompiledFunctionStack.Pop()
			CompiledFunctionStack.Push(Cons(val, val2))
		case LIST2:
			val2 = CompiledFunctionStack.Pop()
			val = CompiledFunctionStack.Pop()
			CompiledFunctionStack.Push(InternalMakeList(val, val2))
		case EQV:
			val2 = CompiledFunctionStack.Pop()
			val = CompiledFunctionStack.Pop()
			CompiledFunctionStack.Push(BooleanWithValue(IsEqv(val, val2)))
		case NEQV:
			val2 = CompiledFunctionStack.Pop()
			val = CompiledFunctionStack.Pop()
			CompiledFunctionStack.Push(BooleanWithValue(!IsEqv(val, val2)))
		case EQ:
			val2 = CompiledFunctionStack.Pop()
			val = CompiledFunctionStack.Pop()
			CompiledFunctionStack.Push(BooleanWithValue(IsEq(val, val2)))
		case NEQ:
			val2 = CompiledFunctionStack.Pop()
			val = CompiledFunctionStack.Pop()
			CompiledFunctionStack.Push(BooleanWithValue(!IsEq(val, val2)))
		case EQUAL:
			val2 = CompiledFunctionStack.Pop()
			val = CompiledFunctionStack.Pop()
			CompiledFunctionStack.Push(BooleanWithValue(IsEqual(val, val2)))
		case NEQUAL:
			val2 = CompiledFunctionStack.Pop()
			val = CompiledFunctionStack.Pop()
			CompiledFunctionStack.Push(BooleanWithValue(!IsEqual(val, val2)))
		case LIST3:
			val3 = CompiledFunctionStack.Pop()
			val2 = CompiledFunctionStack.Pop()
			val = CompiledFunctionStack.Pop()
			CompiledFunctionStack.Push(InternalMakeList(val, val2, val3))
		case NAMEBANG:
			// pop the name
			val = CompiledFunctionStack.Pop()
			// pop the function
			val2 = CompiledFunctionStack.Pop()
			CompiledFunctionValue(val2).Name = StringValue(val)
			CompiledFunctionStack.Push(val2)
		case HALT:
			return CompiledFunctionStack.Top(), nil
		}
	}
}
