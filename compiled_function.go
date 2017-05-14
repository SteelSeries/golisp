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
	MakePrimitiveFunction("new-fn", "4", newFnImpl)
	MakePrimitiveFunction("compiled-name", "1", getCompiledNameImpl)
	MakePrimitiveFunction("compiled-name!", "2", setCompiledNameImpl)
	MakePrimitiveFunction("compiled-code", "1", getCompiledCodeImpl)
	MakePrimitiveFunction("compiled-code!", "2", setCompiledCodeImpl)
	MakePrimitiveFunction("compiled-env", "1", getCompiledEnvImpl)
	MakePrimitiveFunction("compiled-env!", "2", setCompiledEnvImpl)
}

func newFnImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	name := First(args)
	if !StringP(name) && !SymbolP(name) {
		err = ProcessError(fmt.Sprintf("new-fn expects a string or symbol name, but recieved %s.", String(name)), env)
		return
	}

	localEnv := Second(args)
	if !ListP(localEnv) {
		err = ProcessError(fmt.Sprintf("new-fn expects a list for local environment, but recieved %s.", String(localEnv)), env)
		return
	}

	argList := Third(args)
	if !ListP(argList) {
		err = ProcessError(fmt.Sprintf("new-fn expects a list for arguments, but recieved %s.", String(argList)), env)
		return
	}

	code := Fourth(args)
	if !VectorP(code) && !ListP(code) {
		err = ProcessError(fmt.Sprintf("new-fn expects a vector or list of code, but recieved %s.", String(code)), env)
		return
	}

	return CompiledFunctionWithValue(MakeCompiledFunction(StringValue(name), localEnv, argList, code)), nil
}

func getCompiledNameImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	if !CompiledFunctionP(f) {
		err = ProcessError(fmt.Sprintf("compiled-name expects a compiled function, but recieved %s.", String(f)), env)
		return
	}
	result = StringWithValue(CompiledFunctionValue(f).Name)
	return
}

func setCompiledNameImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	if !CompiledFunctionP(f) {
		err = ProcessError(fmt.Sprintf("compiled-name! expects a compiled function, but recieved %s.", String(f)), env)
		return
	}
	name := Second(args)
	if !StringP(name) && !SymbolP(name) {
		err = ProcessError(fmt.Sprintf("compiled-name! expects a compiled function, but recieved %s.", String(name)), env)
		return
	}
	CompiledFunctionValue(f).Name = StringValue(name)
	return
}

func getCompiledCodeImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	if !CompiledFunctionP(f) {
		err = ProcessError(fmt.Sprintf("compiled-code expects a compiled function, but recieved %s.", String(f)), env)
		return
	}
	result = CompiledFunctionValue(f).Code
	return
}

func setCompiledCodeImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	if !CompiledFunctionP(f) {
		err = ProcessError(fmt.Sprintf("compiled-code! expects a compiled function as its first argument, but recieved %s.", String(f)), env)
		return
	}
	code := Second(args)
	if !ListP(code) && !VectorP(code) {
		err = ProcessError(fmt.Sprintf("compiled-code! expects a list or vector of code as its second argument, but recieved %s.", String(code)), env)
		return
	}
	CompiledFunctionValue(f).Code = code
	return
}

func getCompiledEnvImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	if !CompiledFunctionP(f) {
		err = ProcessError(fmt.Sprintf("compiled-env expects a compiled function, but recieved %s.", String(f)), env)
		return
	}
	result = CompiledFunctionValue(f).Env
	return
}

func setCompiledEnvImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	if !CompiledFunctionP(f) {
		err = ProcessError(fmt.Sprintf("compiled-env! expects a compiled function as its first argument, but recieved %s.", String(f)), env)
		return
	}
	localEnv := Second(args)
	if !ListP(localEnv) {
		err = ProcessError(fmt.Sprintf("compiled-env! expects a list as its second argument, but recieved %s.", String(localEnv)), env)
		return
	}
	CompiledFunctionValue(f).Env = localEnv
	return
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
