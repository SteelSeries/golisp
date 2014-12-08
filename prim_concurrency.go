// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the concurrency primitive functions.

package golisp

import (
	"fmt"
	"time"
	"unsafe"
)

type Process struct {
	Env   *SymbolTableFrame
	Code  *Data
	Wake  chan bool
	Abort chan bool
}

func RegisterConcurrencyPrimitives() {
	MakePrimitiveFunction("fork", 1, ForkImpl)
	MakePrimitiveFunction("proc-sleep", 2, ProcSleepImpl)
	MakePrimitiveFunction("wake", 1, WakeImpl)
	MakePrimitiveFunction("schedule", 2, ScheduleImpl)
	MakePrimitiveFunction("abandon", 1, AbandonImpl)
}

func ForkImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f, err := Eval(Car(args), env)
	if err != nil {
		return
	}

	if !FunctionP(f) {
		err = ProcessError(fmt.Sprintf("fork expected a function, but received %v.", f), env)
		return
	}

	if f.Func.RequiredArgCount != 1 {
		err = ProcessError(fmt.Sprintf("fork expected a function with arity of 1, but it was %d.", f.Func.RequiredArgCount), env)
		return
	}

	proc := &Process{Env: env, Code: f, Wake: make(chan bool), Abort: make(chan bool)}
	procObj := ObjectWithTypeAndValue("Process", unsafe.Pointer(&proc))

	go func() {
		_, err = f.Func.ApplyWithoutEval(InternalMakeList(procObj), env)
	}()

	return procObj, nil
}

func ProcSleepImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	procObj, err := Eval(Car(args), env)
	if err != nil {
		return
	}

	if !ObjectP(procObj) || TypeOfObject(procObj) != "Process" {
		err = ProcessError(fmt.Sprintf("procsleep expects a Process object expected but received %s.", TypeOfObject(procObj)), env)
		return
	}

	proc := (*Process)(ObjectValue(procObj))

	fmt.Printf("%v\n", proc)

	millis, err := Eval(Cadr(args), env)
	if err != nil {
		return
	}
	if !IntegerP(millis) {
		err = ProcessError(fmt.Sprintf("procsleep expected an integer as a delay, but received %v.", millis), env)
		return
	}

	fmt.Printf("%d\n", IntegerValue(millis))

	timeout := time.After(time.Duration(IntegerValue(millis)) * time.Millisecond)

	var woken = false
	select {
	case <-proc.Wake:
		woken = true
	case <-timeout:
		woken = false
	}

	return BooleanWithValue(woken), nil
}

func WakeImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	procObj, err := Eval(Car(args), env)
	if err != nil {
		return
	}

	if !ObjectP(procObj) || TypeOfObject(procObj) != "Process" {
		err = ProcessError(fmt.Sprintf("procsleep expects a Process object expected but received %s.", TypeOfObject(procObj)), env)
		return
	}

	proc := (*Process)(ObjectValue(procObj))

	proc.Wake <- true

	return StringWithValue("OK"), nil
}

func ScheduleImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return
}

func AbandonImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return
}
