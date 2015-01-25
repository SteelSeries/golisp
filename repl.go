// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file provides a REPL.

package golisp

import (
	"container/list"
	"fmt"
)

func Repl() {
	IsInteractive = true
	fmt.Printf("Welcome to GoLisp\n")
	fmt.Printf("Copyright 2014 SteelSeries\n")
	fmt.Printf("Evaluate '(quit)' to exit.\n\n")
	prompt := "> "
	LoadHistoryFromFile(".golisp_history")
	lastInput := ""
	for true {
		defer func() {
			if x := recover(); x != nil {
				fmt.Printf("Don't Panic! %v\n", x)
			}
		}()
		DebugCurrentFrame = nil
		DebugSingleStep = false
		DebugEvalInDebugRepl = false
		Global.CurrentCode = list.New()
		inputp := ReadLine(&prompt)
		fmt.Printf("inputp: %v\n", inputp)
		if inputp == nil {
			QuitImpl(nil, nil)
		} else {
			input := *inputp
			fmt.Printf("input: <%s>\n", inputp)
			if input != "" {
				if input != lastInput {
					AddHistory(input)
				}
				lastInput = input
				code, err := Parse(input)
				if err != nil {
					fmt.Printf("Error: %s\n", err)
				} else {
					d, err := Eval(code, Global)
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
