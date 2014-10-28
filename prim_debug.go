// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the debugging primitive functions.

package golisp

import (
	"fmt"
	"strings"
)

var DebugCommandPrefix string = ":"

func RegisterDebugPrimitives() {
	MakePrimitiveFunction("debug-trace", -1, DebugTraceImpl)
	MakePrimitiveFunction("debug", -1, DebugImpl)
	MakePrimitiveFunction("dump", 0, DumpSymbolTableImpl)
}

func DumpSymbolTableImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	env.Dump()
	return
}

func DebugTraceImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if Length(args) == 1 {
		DebugTrace = BooleanValue(Car(args))
	}
	return BooleanWithValue(DebugTrace), nil
}

func DebugImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	fmt.Printf("Debugger\n")

	DebugRepl(env.Parent)
	return
}

func DebugRepl(env *SymbolTableFrame) {
	env.DumpHeader()
	prompt := "D> "
	for true {
		defer func() {
			if x := recover(); x != nil {
				println("BANG!")
			}
		}()
		input := *ReadLine(&prompt)
		if input != "" {
			if strings.HasPrefix(input, DebugCommandPrefix) {
				cmd := strings.TrimPrefix(input, DebugCommandPrefix)
				tokens := strings.Split(cmd, " ")
				switch tokens[0] {
				case "d":
					env.Dump()
				case "b":
					env.DumpHeaders()
					fmt.Printf("\n")
				case "f":
					var fnum int
					if len(tokens) != 2 {
						fmt.Printf("Missing frame number.\n")
					} else {
						_, err := fmt.Sscanf(tokens[1], "%d", &fnum)
						if err != nil {
							fmt.Printf("Bad frame number: '%s'. %s\n", tokens[1], err)
						} else {
							env.DumpSingleFrame(fnum)
						}
					}
				case "c":
					DebugCurrentFrame = nil
					DebugSingleStep = false
					DebugEvalInDebugRepl = false
					return
				case "u":
					DebugCurrentFrame = env
					return
				case "s":
					DebugSingleStep = true
					return
				case "q":
					QuitImpl(nil, nil)
				}
			} else {
				code, err := Parse(input)
				if err != nil {
					fmt.Printf("Error: %s\n", err)
				} else {
					DebugEvalInDebugRepl = true
					d, err := Eval(code, env)
					DebugEvalInDebugRepl = false
					if err != nil {
						fmt.Printf("Error in evaluation: %s\n", err)
					} else {
						fmt.Printf("==> %s\n", String(d))
					}
				}
			}
		}
	}
}
