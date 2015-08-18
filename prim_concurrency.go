// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the concurrency primitive functions.

package golisp

import (
	"fmt"
	"runtime"
	"strings"
	"time"
	"unsafe"
)

type Process struct {
	Env           *SymbolTableFrame
	Code          *Data
	Wake          chan bool
	Abort         chan bool
	Restart       chan bool
	ReturnValue   chan *Data
	Joined        bool
	ScheduleTimer *time.Timer
}

func RegisterConcurrencyPrimitives() {
	MakePrimitiveFunction("fork", "1", ForkImpl)
	MakePrimitiveFunction("proc-sleep", "2", ProcSleepImpl)
	MakePrimitiveFunction("wake", "1", WakeImpl)
	MakePrimitiveFunction("schedule", "2", ScheduleImpl)
	MakePrimitiveFunction("reset-timeout", "1", ResetTimeoutImpl)
	MakePrimitiveFunction("abandon", "1", AbandonImpl)
	MakePrimitiveFunction("join", "1", JoinImpl)
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

	proc := &Process{Env: env, Code: f, Wake: make(chan bool, 1), Abort: make(chan bool, 1), Restart: make(chan bool, 1), ReturnValue: make(chan *Data, 1)}
	procObj := ObjectWithTypeAndValue("Process", unsafe.Pointer(proc))

	go func() {
		var returnValue *Data = nil
		defer func() {
			proc.ReturnValue <- returnValue
		}()

		callWithPanicProtection(func() {
			var forkedErr error
			returnValue, forkedErr = FunctionValue(f).ApplyWithoutEval(InternalMakeList(procObj), env)
			if forkedErr != nil {
				fmt.Println(forkedErr)
			}
		}, "fork")
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
		callWithPanicProtection(func() {
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
		}, "schedule")
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
	var str string
	select {
	case proc.Restart <- true:
		str = "OK"
	default:
		str = "task was already completed or abandoned"
	}
	return StringWithValue(str), nil
}

func JoinImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	procObj := Car(args)

	if !ObjectP(procObj) || ObjectType(procObj) != "Process" {
		err = ProcessError(fmt.Sprintf("join expects a Process object expected but received %s.", ObjectType(procObj)), env)
		return
	}
	proc := (*Process)(ObjectValue(procObj))

	if !proc.Joined {
		proc.Joined = true
		return <-proc.ReturnValue, nil
	} else {
		return nil, ProcessError("tried to run join on a task twice", env)
	}
}

func callWithPanicProtection(f func(), prefix string) {
	defer func() {
		if recovered := recover(); recovered != nil {
			stackBuf := make([]byte, 10000)
			stackBuf = stackBuf[:runtime.Stack(stackBuf, false)]
			stack := strings.Split(string(stackBuf), "\n")
			for i := 0; i < 7; i++ {
				fmt.Println(stack[i])
			}
		}
	}()

	f()
}
