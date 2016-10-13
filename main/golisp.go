// Copyright 2013 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file provides a repl
package main

import (
	"flag"
	"fmt"
	. "github.com/dastels/golisp"
	"os"
	"path/filepath"
	"strings"
)

var (
	runTests     bool = false
	verboseTests bool = false
	runRepl bool = false
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

func loadFile(path string) error {
	absPath, err := filepath.Abs(path)
	if err != nil {
		fmt.Printf(" - Error: %s\n", err)
		return err
	}
	if !(filepath.Base(absPath) == ".") && !(filepath.Base(absPath) == "..") {
		extension := filepath.Ext(path)
		if extension == ".scm" || extension == ".lsp" {
			fmt.Printf("  Loading %s", path)
			_, err = ProcessFile(absPath)
			if err != nil {
				fmt.Printf(" - Error: %s\n", err)
				return err
			} else {
				fmt.Printf("\n")
			}
		}
	}
	return nil
}


func walkFunc(path string, info os.FileInfo, err error) error {
	return loadFile(path)
}

func main() {
	flag.BoolVar(&runRepl, "r", false, "Whether to run the repl after loading golisp code.  Defaults to false.")
	flag.BoolVar(&runTests, "t", false, "Whether to run tests and exit.  Defaults to false.")
	flag.BoolVar(&verboseTests, "v", false, "Whether tests should be verbose.  Defaults to false.")
	flag.Parse()

	fmt.Printf("Loading the lisp directory\n")
	walkErr := filepath.Walk("lisp", walkFunc)
	if walkErr != nil {
		fmt.Printf("Error loading code\n")
		return
	}

	if runTests {
		test()
	} else {
		var programArgs []string
		for i := 0; i < flag.NArg(); i = i + 1 {
			if flag.Arg(i) == "--" {
				programArgs = flag.Args()[i+1:]
				break
			} else {
				name, err := filepath.Abs(flag.Arg(i))
				if err != nil {
					fmt.Printf(" - Error: %s\n", err)
					return
				}
				f, err := os.Open(name)
				if err != nil {
					fmt.Println(err)
					return
				}
				defer f.Close()
				fi, err := f.Stat()
				if err != nil {
					fmt.Println(err)
					return
				}
				switch mode := fi.Mode(); {
				case mode.IsDir():
					walkErr = filepath.Walk(flag.Arg(i), walkFunc)
					if walkErr != nil {
						fmt.Printf("Error loading code\n")
						return
					}
				case mode.IsRegular():
					loadErr := loadFile(flag.Arg(i))
					if loadErr != nil {
						return
					}
				}
			}
		}
		
		mainValue := Global.ValueOf(Intern("main"))

		if runRepl || !FunctionP(mainValue) {
			Repl()
		} else {
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
		}
	}
}
