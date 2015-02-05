// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the system primitive functions.

package golisp

import (
	"fmt"
	"os"
	"strings"
	"time"
)

func RegisterSystemPrimitives() {
	MakePrimitiveFunction("load", 1, LoadFileImpl)
	MakePrimitiveFunction("sleep", 1, SleepImpl)
	MakePrimitiveFunction("millis", 0, MillisImpl)
	MakePrimitiveFunction("write-line", 1, WriteLineImpl)
	MakePrimitiveFunction("str", -1, MakeStringImpl)
	MakePrimitiveFunction("intern", 1, InternImpl)
	MakePrimitiveFunction("time", 1, TimeImpl)
	MakePrimitiveFunction("quit", 0, QuitImpl)
}

func LoadFileImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	filename := Eval(Car(args), env)
	if !StringP(filename) {
		err = ProcessError("Filename must be a string", env)
		return
	}

	return ProcessFile(StringValue(filename))
}

func QuitImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	WriteHistoryToFile(".golisp_history")
	os.Exit(0)
	return
}

func SleepImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	n, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !IntegerP(n) {
		err = ProcessError(fmt.Sprintf("Number expected, received %s", String(n)), env)
		return
	}
	millis := IntegerValue(n)
	time.Sleep(time.Duration(millis) * time.Millisecond)
	return
}

func MillisImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	result = IntegerWithValue(int64(time.Now().UnixNano() / 1e6))
	return
}

func WriteLineImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	data, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	println(PrintString(data))
	return
}

func MakeStringImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	pieces := make([]string, 2)
	for cell := args; NotNilP(cell); cell = Cdr(cell) {
		sexpr := Car(cell)
		s, err := Eval(sexpr, env)
		if err != nil {
			break
		}
		pieces = append(pieces, PrintString(s))
	}
	return StringWithValue(strings.Join(pieces, "")), nil
}

func TimeImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	fmt.Printf("Starting timer.\n")
	startTime := time.Now()

	for cell := args; NotNilP(cell); cell = Cdr(cell) {
		sexpr := Car(cell)
		result, err = Eval(sexpr, env)
		if err != nil {
			break
		}
	}

	d := time.Since(startTime)
	fmt.Printf("Stopped timer.\nTook %v to run.\n", d)
	result = IntegerWithValue(int64(d.Nanoseconds() / 1000000))
	return
}

func InternImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	sym, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !StringP(sym) {
		err = ProcessError(fmt.Sprintf("intern expects a string, but received %s.", String(sym)), env)
		return
	}

	return SymbolWithName(StringValue(sym)), nil
}
