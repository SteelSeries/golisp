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
	Env           *SymbolTableFrame
	Code          *Data
	Wake          chan bool
	Abort         chan bool
	Restart       chan bool
	ScheduleTimer *time.Timer
}

func RegisterConcurrencyPrimitives() {
	MakePrimitiveFunction("fork", 1, ForkImpl)
	MakePrimitiveFunction("proc-sleep", 2, ProcSleepImpl)
	MakePrimitiveFunction("wake", 1, WakeImpl)
	MakePrimitiveFunction("schedule", 2, ScheduleImpl)
	MakePrimitiveFunction("reset-timeout", 1, ResetTimeoutImpl)
	MakePrimitiveFunction("abandon", 1, AbandonImpl)
}

func ForkImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := Car(args)

	if !FunctionP(f) {
		err = ProcessError(fmt.Sprintf("fork expected a function, but received %v.", f), env)
		return
	}

	if FunctionValue(f).RequiredArgCount != 1 {
		err = ProcessError(fmt.Sprintf("fork expected a function with arity of 1, but it was %d.", FunctionValue(f).RequiredArgCount), env)
		return
	}

	proc := &Process{Env: env, Code: f, Wake: make(chan bool, 1), Abort: make(chan bool, 1), Restart: make(chan bool, 1)}
	procObj := ObjectWithTypeAndValue("Process", unsafe.Pointer(proc))

	go func() {
		_, err = FunctionValue(f).ApplyWithoutEval(InternalMakeList(procObj), env)
	}()

	return procObj, nil
}

func ProcSleepImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	procObj := Car(args)

	if !ObjectP(procObj) || ObjectType(procObj) != "Process" {
		err = ProcessError(fmt.Sprintf("proc-sleep expects a Process object expected but received %s.", ObjectType(procObj)), env)
		return
	}

	proc := (*Process)(ObjectValue(procObj))

	millis := Cadr(args)
	if !IntegerP(millis) {
		err = ProcessError(fmt.Sprintf("proc-sleep expected an integer as a delay, but received %v.", millis), env)
		return
	}

	woken := false
	select {
	case <-proc.Wake:
		woken = true
	case <-time.After(time.Duration(IntegerValue(millis)) * time.Millisecond):
	}

	return BooleanWithValue(woken), nil
}

func WakeImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	procObj := Car(args)

	if !ObjectP(procObj) || ObjectType(procObj) != "Process" {
		err = ProcessError(fmt.Sprintf("wake expects a Process object expected but received %s.", ObjectType(procObj)), env)
		return
	}

	proc := (*Process)(ObjectValue(procObj))
	proc.Wake <- true
	return StringWithValue("OK"), nil
}

func ScheduleImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	millis := Car(args)
	if !IntegerP(millis) {
		err = ProcessError(fmt.Sprintf("schedule expected an integer as a delay, but received %v.", millis), env)
		return
	}
	f := Cadr(args)

	if !FunctionP(f) {
		err = ProcessError(fmt.Sprintf("schedule expected a function, but received %v.", f), env)
		return
	}

	if FunctionValue(f).RequiredArgCount != 1 {
		err = ProcessError(fmt.Sprintf("schedule expected a function with arity of 1, but it was %d.", FunctionValue(f).RequiredArgCount), env)
		return
	}

	proc := &Process{
		Env:           env,
		Code:          f,
		Wake:          make(chan bool, 1),
		Abort:         make(chan bool, 1),
		Restart:       make(chan bool, 1),
		ScheduleTimer: time.NewTimer(time.Duration(IntegerValue(millis)) * time.Millisecond)}
	procObj := ObjectWithTypeAndValue("Process", unsafe.Pointer(proc))

	aborted := false
	go func() {
	Loop:
		for {
			select {
			case <-proc.Abort:
				aborted = true
				break Loop
			case <-proc.Restart:
				proc.ScheduleTimer.Reset(time.Duration(IntegerValue(millis)) * time.Millisecond)
			case <-proc.ScheduleTimer.C:
				_, err = FunctionValue(f).ApplyWithoutEval(InternalMakeList(procObj), env)
				break Loop
			}
		}
	}()

	return procObj, nil

}

func AbandonImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	procObj := Car(args)

	if !ObjectP(procObj) || ObjectType(procObj) != "Process" {
		err = ProcessError(fmt.Sprintf("adandon expects a Process object expected but received %s.", ObjectType(procObj)), env)
		return
	}

	proc := (*Process)(ObjectValue(procObj))
	proc.Abort <- true
	return StringWithValue("OK"), nil
}

func ResetTimeoutImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	procObj := Car(args)

	if !ObjectP(procObj) || ObjectType(procObj) != "Process" {
		err = ProcessError(fmt.Sprintf("restart expects a Process object expected but received %s.", ObjectType(procObj)), env)
		return
	}

	proc := (*Process)(ObjectValue(procObj))
	proc.Restart <- true
	return StringWithValue("OK"), nil
}
