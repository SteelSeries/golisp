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
	"os/exec"
	"runtime"
	"strings"
	"sync"
	"time"
)

var symbolCounts map[string]int = make(map[string]int)
var symbolCountsMutex sync.Mutex

func RegisterSystemPrimitives() {
	Global.BindTo(Intern("__OS__"), Intern(runtime.GOOS))

	MakeTypedPrimitiveFunction("sleep", "1", sleepImpl, []uint32{IntegerType})
	MakePrimitiveFunction("millis", "0", millisImpl)
	MakePrimitiveFunction("write-line", "*", writeLineImpl)
	MakePrimitiveFunction("write-log", "*", writeLogImpl)
	MakePrimitiveFunction("str", "*", makeStringImpl)
	MakePrimitiveFunction("quit", "0", QuitImpl)
	MakeTypedPrimitiveFunction("intern", "1", internImpl, []uint32{StringType})
	MakeTypedPrimitiveFunction("symbol->string", "1", symbolToStringImpl, []uint32{SymbolType})
	MakeTypedPrimitiveFunction("gensym", "0|1", gensymImpl, []uint32{StringType | SymbolType})
	MakeTypedPrimitiveFunction("gensym-naked", "0|1", gensymNakedImpl, []uint32{StringType | SymbolType})
	MakeTypedPrimitiveFunction("eval", "1|2", evalImpl, []uint32{AnyType, EnvironmentType})
	MakeTypedPrimitiveFunction("load", "1", loadFileImpl, []uint32{StringType})
	MakeTypedPrimitiveFunction("load-locally", "1", loadFileLocallyImpl, []uint32{StringType})
	MakeTypedPrimitiveFunction("load-in-environment", "2", loadFileInEnvironmentImpl, []uint32{StringType, EnvironmentType})
	MakePrimitiveFunction("global-eval", "1", globalEvalImpl)
	MakePrimitiveFunction("panic!", "1", panicImpl)
	MakePrimitiveFunction("error", "1", errorImpl)
	MakeTypedPrimitiveFunction("exec", ">=1", execImpl, []uint32{StringType, AnyType})
	MakeTypedPrimitiveFunction("get-env", "1", getEnvImpl, []uint32{StringType | SymbolType})
	MakeTypedPrimitiveFunction("set-env", "2", setEnvImpl, []uint32{StringType | SymbolType, StringType | SymbolType})
	MakeTypedPrimitiveFunction("unset-env", "1", unsetEnvImpl, []uint32{StringType | SymbolType})
	MakeSpecialForm("on-error", "2|3", onErrorImpl)
	MakeSpecialForm("time", "1", timeImpl)
	MakeSpecialForm("profile", "1|2", profileImpl)
}

func loadFileImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return ProcessFile(StringValue(First(args)))
}

func loadFileLocallyImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return ProcessFileInEnvironment(StringValue(First(args)), env)
}

func loadFileInEnvironmentImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return ProcessFileInEnvironment(StringValue(First(args)), EnvironmentValue(Second(args)))
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
	"later dude",
}

func panicImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	panic(String(First(args)))
}

func errorImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return nil, ProcessError(PrintString(First(args)), env)
}

func onErrorImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	result, errThrown := Eval(First(args), env)
	if errThrown == nil {
		if Length(args) == 3 {
			f, err := Eval(Third(args), env)
			if err != nil {
				return nil, err
			}
			if !FunctionP(f) {
				return nil, ProcessError("on-error requires a function as it's third argument", env)
			}
			noErrHandler := FunctionValue(f)
			return noErrHandler.Apply(nil, env)

		} else {
			return
		}
	}

	f, err := Eval(Second(args), env)
	if err != nil {
		return
	}

	if !FunctionP(f) {
		err = ProcessError("on-error requires a function as it's second argument", env)
		return
	}
	handler := FunctionValue(f)
	errString := StringWithValue(errThrown.Error())
	return handler.Apply(InternalMakeList(errString), env)
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

func sleepImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	millis := IntegerValue(First(args))
	time.Sleep(time.Duration(millis) * time.Millisecond)
	return
}

func millisImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	now := time.Now()
	hour := now.Hour()
	minute := now.Minute()
	second := now.Second()
	milli := now.Nanosecond() / 1000000
	result = IntegerWithValue(int64(hour*3600000 + minute*60000 + second*1000 + milli))
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

func writeLineImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	println(concatStringForms(args))
	return
}

func writeLogImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	LogPrintf("%s\r\n", concatStringForms(args))
	return
}

func makeStringImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return StringWithValue(concatStringForms(args)), nil
}

func timeImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	startTime := time.Now()

	for cell := args; NotNilP(cell); cell = Cdr(cell) {
		sexpr := Car(cell)
		result, err = Eval(sexpr, env)
		if err != nil {
			break
		}
	}

	d := time.Since(startTime)
	result = IntegerWithValue(int64(d.Nanoseconds() / 1000000))
	return
}

func internImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return Intern(StringValue(First(args))), nil
}

func symbolToStringImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return StringWithValue(StringValue(First(args))), nil
}

func gensymHelper(primitiveName string, args *Data, env *SymbolTableFrame) (prefix string, count int, err error) {
	if Length(args) > 1 {
		err = ProcessError(fmt.Sprintf("%s expects 0 or 1 argument, but received %d.", primitiveName, Length(args)), env)
		return
	}

	if Length(args) == 0 {
		prefix = "GENSYM"
	} else {
		arg := Car(args)
		if !StringP(arg) && !SymbolP(arg) {
			err = ProcessError(fmt.Sprintf("%s expects a string or symbol, but recieved %s.", primitiveName, String(arg)), env)
			return
		}
		prefix = StringValue(arg)
	}

	symbolCountsMutex.Lock()
	count = symbolCounts[prefix]
	if count == 0 {
		count = 1
	} else {
		count += 1
	}
	symbolCounts[prefix] = count
	symbolCountsMutex.Unlock()
	return
}

func gensymImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	prefix, count, err := gensymHelper("gensym", args, env)
	if err != nil {
		return
	}
	result = SymbolWithName(fmt.Sprintf("%s-%d", prefix, count))
	return
}

func gensymNakedImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	prefix, count, err := gensymHelper("gensym-naked", args, env)
	if err != nil {
		return
	}
	result = NakedSymbolWithName(fmt.Sprintf("%s-%d", prefix, count))
	return
}

func evalImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var evalEnv *SymbolTableFrame
	if Length(args) == 2 {
		evalEnv = EnvironmentValue(Second(args))
	} else {
		evalEnv = env
	}
	return Eval(First(args), evalEnv)
}

func globalEvalImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return Eval(First(args), Global)
}

func profileImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if Length(args) == 2 {
		if !StringP(Cadr(args)) {
			err = ProcessError(fmt.Sprintf("profile requires a string filename, but received %s.", String(Cadr(args))), env)
		}
		StartProfiling(StringValue(Cadr(args)))
	} else {
		StartProfiling("")
	}

	result, err = Eval(Car(args), env)

	EndProfiling()

	return
}

func execImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	cmdString := StringValue(First(args))
	cmdArgs := make([]string, 0, Length(args)-1)

	var cmd *exec.Cmd

	if Length(args) > 1 {
		for cell := Cdr(args); !NilP(cell); cell = Cdr(cell) {
			value := Car(cell)
			if StringP(value) || SymbolP(value) {
				cmdArgs = append(cmdArgs, StringValue(value))
			} else {
				cmdArgs = append(cmdArgs, String(value))
			}
		}
		cmd = exec.Command(cmdString, cmdArgs...)
	} else {
		cmd = exec.Command(cmdString)
	}
	err = cmd.Start()
	return
}

func getEnvImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	result = StringWithValue(os.Getenv(StringValue(First(args))))
	return
}

func setEnvImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	err = os.Setenv(StringValue(First(args)), StringValue(Second(args)))
	return
}

func unsetEnvImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	err = os.Unsetenv(StringValue(First(args)))
	return
}
