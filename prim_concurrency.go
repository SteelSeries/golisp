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
	"sync/atomic"
	"time"
	"unsafe"
)

type empty struct{}

type Process struct {
	Env           *SymbolTableFrame
	Code          *Data
	Wake          chan empty
	Abort         chan empty
	Restart       chan empty
	ReturnValue   chan *Data
	Joined        int32
	ScheduleTimer *time.Timer
}

func RegisterConcurrencyPrimitives() {
	MakePrimitiveFunction("fork", ">=1", ForkImpl)
	MakePrimitiveFunction("proc-sleep", "2", ProcSleepImpl)
	MakePrimitiveFunction("wake", "1", WakeImpl)
	MakePrimitiveFunction("schedule", ">=2", ScheduleImpl)
	MakePrimitiveFunction("reset-timeout", "1", ResetTimeoutImpl)
	MakePrimitiveFunction("abandon", "1", AbandonImpl)
	MakePrimitiveFunction("join", "1", JoinImpl)

	MakePrimitiveFunction("atomic", "0|1", AtomicImpl)
	MakePrimitiveFunction("atomic-load", "1", AtomicLoadImpl)
	MakePrimitiveFunction("atomic-store!", "2", AtomicStoreImpl)
	MakePrimitiveFunction("atomic-add!", "2", AtomicAddImpl)
	MakePrimitiveFunction("atomic-swap!", "2", AtomicSwapImpl)
	MakePrimitiveFunction("atomic-compare-and-swap!", "3", AtomicCompareAndSwapImpl)
}

func ForkImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := Car(args)

	if !FunctionP(f) {
		err = ProcessError(fmt.Sprintf("fork expected a function, but received %v.", f), env)
		return
	}

	argsCount := Length(Cdr(args)) + 1
	function := FunctionValue(f)

	if function.VarArgs {
		if argsCount < function.RequiredArgCount {
			return nil, ProcessError(fmt.Sprintf("fork expected a function with arity of at most %d, but it was %d.", argsCount, function.RequiredArgCount), env)
		}
	} else {
		if argsCount != function.RequiredArgCount {
			return nil, ProcessError(fmt.Sprintf("fork expected a function with arity of %d, but it was %d.", argsCount, function.RequiredArgCount), env)
		}
	}

	proc := &Process{
		Env:         env,
		Code:        f,
		Wake:        make(chan empty, 1),
		Abort:       make(chan empty, 1),
		Restart:     make(chan empty, 1),
		ReturnValue: make(chan *Data, 1)}
	procObj := ObjectWithTypeAndValue("Process", unsafe.Pointer(proc))

	go func() {
		var returnValue *Data
		defer func() {
			proc.ReturnValue <- returnValue
		}()

		callWithPanicProtection(func() {
			var forkedErr error
			returnValue, forkedErr = function.ApplyWithoutEval(Cons(procObj, Cdr(args)), env)
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
	select {
	case proc.Wake <- empty{}:
	default:
	}
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

	argsCount := Length(Cddr(args)) + 1
	function := FunctionValue(f)

	if function.VarArgs {
		if argsCount < function.RequiredArgCount {
			return nil, ProcessError(fmt.Sprintf("schedule expected a function with arity of at most %d, but it was %d.", argsCount, function.RequiredArgCount), env)
		}
	} else {
		if argsCount != function.RequiredArgCount {
			return nil, ProcessError(fmt.Sprintf("schedule expected a function with arity of %d, but it was %d.", argsCount, function.RequiredArgCount), env)
		}
	}

	proc := &Process{
		Env:           env,
		Code:          f,
		Wake:          make(chan empty, 1),
		Abort:         make(chan empty, 1),
		Restart:       make(chan empty, 1),
		ReturnValue:   make(chan *Data, 1),
		ScheduleTimer: time.NewTimer(time.Duration(IntegerValue(millis)) * time.Millisecond)}
	procObj := ObjectWithTypeAndValue("Process", unsafe.Pointer(proc))

	aborted := false
	go func() {
		var returnValue *Data
		defer func() {
			proc.ReturnValue <- returnValue
		}()
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
					var forkedErr error
					returnValue, forkedErr = function.ApplyWithoutEval(Cons(procObj, Cddr(args)), env)
					if forkedErr != nil {
						fmt.Println(forkedErr)
					}
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

	if proc.ScheduleTimer == nil {
		return nil, ProcessError("tried to adandon a Process that isn't scheduled", env)
	}

	select {
	case proc.Abort <- empty{}:
	default:
	}
	return StringWithValue("OK"), nil
}

func ResetTimeoutImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	procObj := Car(args)

	if !ObjectP(procObj) || ObjectType(procObj) != "Process" {
		err = ProcessError(fmt.Sprintf("restart expects a Process object expected but received %s.", ObjectType(procObj)), env)
		return
	}

	proc := (*Process)(ObjectValue(procObj))

	if proc.ScheduleTimer == nil {
		return nil, ProcessError("tried to reset a Process that isn't scheduled", env)
	}

	var str string
	select {
	case proc.Restart <- empty{}:
		str = "OK"
	default:
		str = "task was already completed or abandoned"
	}
	return StringWithValue(str), nil
}

func JoinImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	procObj := Car(args)

	if !ObjectP(procObj) || ObjectType(procObj) != "Process" {
		err = ProcessError(fmt.Sprintf("join expects a Process object but received %s.", ObjectType(procObj)), env)
		return
	}
	proc := (*Process)(ObjectValue(procObj))

	if atomic.CompareAndSwapInt32(&proc.Joined, 0, 1) {
		return <-proc.ReturnValue, nil
	}

	return nil, ProcessError("tried to join on a task twice", env)
}

func AtomicImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	atomicVal := int64(0)

	if Length(args) == 1 {
		initObj := Car(args)
		if !IntegerP(initObj) {
			err = ProcessError(fmt.Sprintf("atomic expects an Integer as it's argument but received %s.", TypeName(TypeOf(initObj))), env)
			return
		}
		atomicVal = IntegerValue(initObj)
	}

	return ObjectWithTypeAndValue("Atomic", unsafe.Pointer(&atomicVal)), nil
}

func AtomicLoadImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	atomicObj := Car(args)
	if !ObjectP(atomicObj) || ObjectType(atomicObj) != "Atomic" {
		err = ProcessError(fmt.Sprintf("atomic-load expects an Atomic object but received %s.", ObjectType(atomicObj)), env)
		return
	}

	pointer := (*int64)(ObjectValue(atomicObj))

	value := atomic.LoadInt64(pointer)

	return IntegerWithValue(value), nil
}

func AtomicStoreImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	atomicObj := Car(args)
	if !ObjectP(atomicObj) || ObjectType(atomicObj) != "Atomic" {
		err = ProcessError(fmt.Sprintf("atomic-store! expects an Atomic object but received %s.", ObjectType(atomicObj)), env)
		return
	}

	pointer := (*int64)(ObjectValue(atomicObj))

	newObj := Cadr(args)

	if !IntegerP(newObj) {
		err = ProcessError(fmt.Sprintf("atomic-store! expects an Integer as it's second argument but received %s.", TypeName(TypeOf(newObj))), env)
		return
	}

	new := IntegerValue(newObj)

	atomic.StoreInt64(pointer, new)

	return
}

func AtomicAddImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	atomicObj := Car(args)
	if !ObjectP(atomicObj) || ObjectType(atomicObj) != "Atomic" {
		err = ProcessError(fmt.Sprintf("atomic-add! expects an Atomic object but received %s.", ObjectType(atomicObj)), env)
		return
	}

	pointer := (*int64)(ObjectValue(atomicObj))

	deltaObj := Cadr(args)

	if !IntegerP(deltaObj) {
		err = ProcessError(fmt.Sprintf("atomic-add! expects an Integer as it's second argument but received %s.", TypeName(TypeOf(deltaObj))), env)
		return
	}

	delta := IntegerValue(deltaObj)

	new := atomic.AddInt64(pointer, delta)

	return IntegerWithValue(new), nil
}

func AtomicSwapImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	atomicObj := Car(args)
	if !ObjectP(atomicObj) || ObjectType(atomicObj) != "Atomic" {
		err = ProcessError(fmt.Sprintf("atomic-swap! expects an Atomic object but received %s.", ObjectType(atomicObj)), env)
		return
	}

	pointer := (*int64)(ObjectValue(atomicObj))

	newObj := Cadr(args)

	if !IntegerP(newObj) {
		err = ProcessError(fmt.Sprintf("atomic-swap! expects an Integer as it's second argument but received %s.", TypeName(TypeOf(newObj))), env)
		return
	}

	new := IntegerValue(newObj)

	old := atomic.SwapInt64(pointer, new)

	return IntegerWithValue(old), nil
}

func AtomicCompareAndSwapImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	atomicObj := Car(args)
	if !ObjectP(atomicObj) || ObjectType(atomicObj) != "Atomic" {
		err = ProcessError(fmt.Sprintf("atomic-compare-and-swap! expects an Atomic object but received %s.", ObjectType(atomicObj)), env)
		return
	}

	pointer := (*int64)(ObjectValue(atomicObj))

	oldObj := Cadr(args)

	if !IntegerP(oldObj) {
		err = ProcessError(fmt.Sprintf("atomic-compare-and-swap! expects an Integer as it's second argument but received %s.", TypeName(TypeOf(oldObj))), env)
		return
	}

	newObj := Caddr(args)

	if !IntegerP(newObj) {
		err = ProcessError(fmt.Sprintf("atomic-compare-and-swap! expects an Integer as it's third argument but received %s.", TypeName(TypeOf(newObj))), env)
		return
	}

	old := IntegerValue(oldObj)
	new := IntegerValue(newObj)

	swapped := atomic.CompareAndSwapInt64(pointer, old, new)

	return BooleanWithValue(swapped), nil
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
