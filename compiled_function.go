// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file implements compiled functions.

package golisp

import (
	"fmt"
)

type CompiledFunction struct {
	Name string
	Env  *Data
	Args *Data
	Code *Data
}

func RegisterCompiledFunctionPrimitives() {
	MakeTypedPrimitiveFunction("make-compiled-function", "4", newFnImpl, []uint32{StringType | SymbolType, ConsCellType, ConsCellType, ConsCellType | VectorType})
	MakeTypedPrimitiveFunction("compiled-name", "1", getCompiledNameImpl, []uint32{CompiledFunctionType})
	MakeTypedPrimitiveFunction("compiled-name!", "2", setCompiledNameImpl, []uint32{CompiledFunctionType, StringType | SymbolType})
	MakeTypedPrimitiveFunction("compiled-code", "1", getCompiledCodeImpl, []uint32{CompiledFunctionType})
	MakeTypedPrimitiveFunction("compiled-code!", "2", setCompiledCodeImpl, []uint32{CompiledFunctionType, ConsCellType | VectorType})
	MakeTypedPrimitiveFunction("compiled-env", "1", getCompiledEnvImpl, []uint32{CompiledFunctionType})
	MakeTypedPrimitiveFunction("compiled-env!", "2", setCompiledEnvImpl, []uint32{CompiledFunctionType, ConsCellType})
	MakeTypedPrimitiveFunction("compiled-args", "1", getCompiledArgsImpl, []uint32{CompiledFunctionType})
	MakeTypedPrimitiveFunction("compiled-args!", "2", setCompiledArgsImpl, []uint32{CompiledFunctionType, ConsCellType})
}

func newFnImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return CompiledFunctionWithValue(MakeCompiledFunction(StringValue(First(args)), Second(args), Third(args), Fourth(args))), nil
}

func getCompiledNameImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return StringWithValue(CompiledFunctionValue(First(args)).Name), nil
}

func setCompiledNameImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	CompiledFunctionValue(First(args)).Name = StringValue(Second(args))
	return First(args), nil
}

func getCompiledCodeImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return CompiledFunctionValue(First(args)).Code, nil
}

func setCompiledCodeImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	CompiledFunctionValue(First(args)).Code = Second(args)
	return First(args), nil
}

func getCompiledEnvImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return CompiledFunctionValue(First(args)).Env, nil
}

func setCompiledEnvImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	CompiledFunctionValue(First(args)).Env = Second(args)
	return First(args), nil
}

func getCompiledArgsImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return CompiledFunctionValue(First(args)).Args, nil
}

func setCompiledArgsImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	CompiledFunctionValue(First(args)).Args = Second(args)
	return First(args), nil
}

func MakeCompiledFunction(name string, env *Data, args *Data, code *Data) *CompiledFunction {
	return &CompiledFunction{Name: name, Env: env, Args: args, Code: code}
}

func (self *CompiledFunction) String() string {
	return fmt.Sprintf("<compiled function: %s>", self.Name)
}

func (self *CompiledFunction) internalApply(args *Data, env *SymbolTableFrame, shouldEval bool) (result *Data, err error) {
	return ExecuteBytecodeWithArgs(self, ToArray(args), env)
}

func (self *CompiledFunction) Apply(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return self.internalApply(args, env, true)
}

func (self *CompiledFunction) ApplyWithoutEval(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return self.internalApply(args, env, false)
}
