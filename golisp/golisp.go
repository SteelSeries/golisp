// Copyright 2013 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpreter for embedding in a go program for scripting.
// This file provides a repl
package main

import (
	"flag"
	"fmt"
	"strings"

	"github.com/SteelSeries/golisp"
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
	golisp.ProcessFile("lisp/testing.lsp")
	golisp.ParseAndEval(testCommand)
}

func main() {
	flag.BoolVar(&runTests, "t", false, "Whether to run tests and exit.  Defaults to false.")
	flag.BoolVar(&verboseTests, "v", false, "Whether tests should be verbose.  Defaults to false.")
	flag.Parse()
	if runTests {
		test()
	} else {
		for i := 0; i < flag.NArg(); i = i + 1 {
			fmt.Printf("Loading %s\n", flag.Arg(i))
			_, err := golisp.ProcessFile(flag.Arg(i))
			if err != nil {
				fmt.Printf("Error: %s\n", err)
			}
		}

		golisp.Repl()
	}
}
