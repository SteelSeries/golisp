// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpreter for embedding in a go program for scripting.
// This file provides a REPL.

package golisp

import (
	"container/list"
	"fmt"
)

func Repl() {
	IsInteractive = true
	fmt.Printf("Welcome to GoLisp 1.0\n")
	fmt.Printf("Copyright 2015 SteelSeries\n")
	fmt.Printf("Evaluate '(quit)' to exit.\n\n")
	prompt := "> "
	LoadHistoryFromFile(".golisp_history")
	lastInput := ""
	replEnv := NewSymbolTableFrameBelow(Global, "Repl")

	defer func() {
		if x := recover(); x != nil {
			fmt.Printf("Don't Panic! %v\n", x)
		}
	}()

	for true {
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
					d, err := Eval(code, replEnv)
					if err != nil {
						fmt.Printf("Error in evaluation: %s\n", err)
						if DebugOnError {
							DebugRepl(DebugErrorEnv)
						}
					} else {
						fmt.Printf("==> %s\n", String(d))
					}
				}
			}
		}
	}
}
