// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the system primitive functions.

package golisp

import (
	"fmt"
	"math/rand"
	"os"
	"strings"
	"time"
)

var symbolCounts map[string]int = make(map[string]int)

func RegisterSystemPrimitives() {
	MakePrimitiveFunction("sleep", "1", SleepImpl)
	MakePrimitiveFunction("millis", "0", MillisImpl)
	MakePrimitiveFunction("newline", "0", NewlineImpl)
	MakePrimitiveFunction("write", "*", WriteImpl)
	MakePrimitiveFunction("write-line", "*", WriteLineImpl)
	MakePrimitiveFunction("write-log", "*", WriteLogImpl)
	MakePrimitiveFunction("str", "*", MakeStringImpl)
	MakePrimitiveFunction("intern", "1", InternImpl)
	MakePrimitiveFunction("quit", "0", QuitImpl)
	MakePrimitiveFunction("gensym", "0|1", GensymImpl)
	MakePrimitiveFunction("eval", "1|2", EvalImpl)

	MakeRestrictedPrimitiveFunction("load", "1", LoadFileImpl)
	MakeRestrictedPrimitiveFunction("global-eval", "1", GlobalEvalImpl)
	MakeRestrictedPrimitiveFunction("panic!", "1", PanicImpl)

	MakeSpecialForm("time", "1", TimeImpl)
}

func LoadFileImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	filename := Car(args)
	if !StringP(filename) {
		err = ProcessError("Filename must be a string", env)
		return
	}

	return ProcessFile(StringValue(filename))
}

var goodbyes []string = []string{
	"goodbye",
	"zai jian",
	"tot ziens",
	"adieu",
	"auf Wiedersehen",
	"shalom",
	"arrivederci",
	"ja mata ne",
	"anyeonghi gasyeo",
	"adues",
	"do svidan'ya",
	"farvel",
	"namárië",
}

func PanicImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	panic(String(Car(args)))
}

func QuitImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if IsInteractive || DebugEvalInDebugRepl {
		WriteHistoryToFile(".golisp_history")
		rand.Seed(time.Now().Unix())
		LogPrintf("\n\n%s\n\n", goodbyes[rand.Intn(len(goodbyes))])
		os.Exit(0)
	}
	return
}

func SleepImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	n := Car(args)
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

func NewlineImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	println("")
	return
}

func concatStringForms(args *Data) (str string) {
	if NilP(args) || Length(args) == 0 {
		return "()"
	}
	pieces := make([]string, 2)
	for cell := args; NotNilP(cell); cell = Cdr(cell) {
		pieces = append(pieces, PrintString(Car(cell)))
	}
	return strings.Join(pieces, "")
}

func WriteImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	print(concatStringForms(args))
	return
}

func WriteLineImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	println(concatStringForms(args))
	return
}

func WriteLogImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	LogPrintf("%s\r\n", concatStringForms(args))
	return
}

func MakeStringImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return StringWithValue(concatStringForms(args)), nil
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
	sym := Car(args)
	if !StringP(sym) {
		err = ProcessError(fmt.Sprintf("intern expects a string, but received %s.", String(sym)), env)
		return
	}

	return Intern(StringValue(sym)), nil
}

func GensymImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var prefix string
	if Length(args) > 1 {
		err = ProcessError(fmt.Sprintf("gensym expects 0 or 1 argument, but received %d.", Length(args)), env)
		return
	}

	if Length(args) == 0 {
		prefix = "GENSYM"
	} else {
		arg := Car(args)
		if !StringP(arg) && !SymbolP(arg) {
			err = ProcessError(fmt.Sprintf("gensym expects a string or symbol, but recieved %s.", String(arg)), env)
			return
		}
		prefix = StringValue(arg)
	}

	count := symbolCounts[prefix]
	if count == 0 {
		count = 1
	} else {
		count += 1
	}
	symbolCounts[prefix] = count
	result = Intern(fmt.Sprintf("%s-%d", prefix, count))
	return
}

func EvalImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var evalEnv *SymbolTableFrame
	sexpr := Car(args)
	if Length(args) == 2 {
		if !EnvironmentP(Cadr(args)) {
			err = ProcessError(fmt.Sprintf("eval expects an environment as it's second argument, but recieved %s.", String(Cadr(args))), env)
			return
		}
		evalEnv = EnvironmentValue(Cadr(args))
	} else {
		evalEnv = env
	}
	return Eval(sexpr, evalEnv)
}

func GlobalEvalImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return Eval(Car(args), Global)
}
