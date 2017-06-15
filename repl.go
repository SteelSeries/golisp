// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file provides a REPL.

package golisp

import (
	"container/list"
	"fmt"
	"runtime/debug"
)

func Repl() {
	IsInteractive = true
	fmt.Printf("Welcome to GoLisp 1.1\n")
	fmt.Printf("Copyright 2013-2016 SteelSeries\n")
	fmt.Printf("Copyright 2016-2017 Dave Astels\n")
	fmt.Printf("Evaluate '(quit)' to exit.\n\n")
	prompt := "> "
	LoadHistoryFromFile(".golisp_history")
	lastInput := ""
	replEnv := NewSymbolTableFrameBelow(Global, "Repl")
	for true {
		defer func() {
			if x := recover(); x != nil {
				fmt.Printf("Don't Panic! %v\n", x)
				debug.PrintStack()
			}
		}()
		DebugCurrentFrame = nil
		DebugSingleStep = false
		DebugEvalInDebugRepl = false
		replEnv.CurrentCode = list.New()
		inputp := ReadLine(&prompt)
		if inputp == nil {
			QuitImpl(nil, nil)
		} else {
			input := *inputp
			//			fmt.Printf("input: <%s>\n", inputp)
			if input != "" {
				code, err := Parse(input)
				if err != nil {
					fmt.Printf("Error: %s\n", err)
				} else {
					if input != lastInput {
						AddHistory(input)
						lastInput = input
					}
					d, err := Eval(code, Global)
					if err != nil {
						fmt.Printf("Error in evaluation: %s\n", err)
						if DebugOnError {
							DebugRepl(Global)
						}
					} else {
						fmt.Printf("==> %s\n", String(d))
					}
				}
			}
		}
	}
}
