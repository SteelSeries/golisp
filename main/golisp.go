// Copyright 2013 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file provides a repl
package main

import (
	"flag"
	"fmt"
	. "github.com/steelseries/golisp"
	"strings"
)

var (
	runTests     bool = false
	verboseTests bool = false
)

func test() {
	verboseFlag := ""
	testFunction := ""
	if verboseTests {
		verboseFlag = " #t"
	}
	testName := flag.Arg(0)

	if strings.HasSuffix(testName, ".lsp") {
		testFunction = "run-test"
	} else {
		testFunction = "run-all-tests"
	}

	testCommand := fmt.Sprintf("(%s \"%s\"%s)", testFunction, testName, verboseFlag)
	ProcessFile("lisp/testing.lsp")
	ParseAndEval(testCommand)
}

func main() {
	flag.BoolVar(&runTests, "t", false, "Whether to run tests and exit.  Defaults to false.")
	flag.BoolVar(&verboseTests, "v", false, "Whether tests should be verbose.  Defaults to false.")
	flag.Parse()
	if runTests {
		test()
	} else {
		var programArgs []string
		for i := 0; i < flag.NArg(); i = i + 1 {
			if flag.Arg(i) == "--" {
				programArgs = flag.Args()[i+1:]
				break
			} else {
				fmt.Printf("Loading %s\n", flag.Arg(i))
				_, err := ProcessFile(flag.Arg(i))
				if err != nil {
					fmt.Printf("Error: %s\n", err)
				}
			}
		}

		mainValue := Global.ValueOf(Intern("main"))

		if FunctionP(mainValue) {
			println("Calling main")
			args := make([]*Data, 0, len(flag.Args()))
			for _, arg := range programArgs {
				args = append(args, StringWithValue(arg))
			}
			argList := Cons(InternalMakeList(Intern("quote"), ArrayToList(args)), nil)
			result, err := FunctionValue(mainValue).Apply(argList, Global)
			if err != nil {
				fmt.Printf("Error: %s\n", err)
			} else {
				fmt.Printf("==> %s\n", String(result))
			}
		} else {
			Repl()
		}
	}
}
