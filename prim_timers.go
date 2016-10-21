// Copyright 2016 Dave Astels.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the Timer/Ticker primitive functions.

package golisp

import (
	"time"
	"unsafe"
)

func RegisterTimerPrimitives() {
	MakePrimitiveFunction("timer", "2", TimerImpl)
	MakePrimitiveFunction("ticker", "2", TickerImpl)
	MakePrimitiveFunction("stop-ticker", "1", StopTickerImpl)
}

func TimerImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	millis := First(args)
	if !IntegerP(millis) {
		err = ProcessError("timer expects its first argument to be an integer", env)
		return
	}

	functionObj := Second(args)
	if !FunctionP(functionObj) {
		err = ProcessError("timer expects its second argument to be an function", env)
		return
	}

	t := time.NewTimer(time.Millisecond * time.Duration(IntegerValue(millis)))

	go func() {
		<-t.C
		FunctionValue(functionObj).Apply(nil, env)
		if !t.Stop() {
			<-t.C
		}
	}()

	result = ObjectWithTypeAndValue("Timer", unsafe.Pointer(&t))
	return
}

func TickerImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	millis := First(args)
	if !IntegerP(millis) {
		err = ProcessError("ticker expects its first argument to be an integer", env)
		return
	}

	functionObj := Second(args)
	if !FunctionP(functionObj) {
		err = ProcessError("ticker expects its second argument to be an function", env)
		return
	}

	t := time.NewTicker(time.Millisecond * time.Duration(IntegerValue(millis)))

	go func() {
		for _ = range t.C {
			FunctionValue(functionObj).Apply(nil, env)
		}
	}()

	result = ObjectWithTypeAndValue("Ticker", unsafe.Pointer(t))
	return
}

func StopTickerImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	tickerObj := First(args)
	if !ObjectP(tickerObj) || ObjectType(tickerObj) != "Ticker" {
		err = ProcessError("stop-ticker expects its argument to be an ticker object", env)
		return
	}

	t := (*time.Ticker)(ObjectValue(tickerObj))
	t.Stop()
	return
}
