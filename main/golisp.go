// Copyright 2013 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file provides a repl
package main

import (
	"flag"
	"fmt"
	"github.com/steelseries/golisp"
	"time"
)

var (
	runTests bool = false
)

func test() {
	startTime := time.Now()
	for i := 0; i < flag.NArg(); i = i + 1 {
		if golisp.VerboseTests {
			fmt.Printf("Loading %s\n", flag.Arg(i))
		}
		_, err := golisp.ProcessFile(flag.Arg(i))
		if err != nil {
			//fmt.Printf("Error: %s\n", err)
		}
	}
	golisp.PrintTestResults(time.Since(startTime))
}

func main() {
	flag.BoolVar(&runTests, "t", false, "Whether to run tests and exit.  Defaults to false.")
	flag.BoolVar(&golisp.VerboseTests, "v", false, "Whether tests should be verbose.  Defaults to false.")
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
