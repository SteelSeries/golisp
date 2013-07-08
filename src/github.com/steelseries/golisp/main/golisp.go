// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file provides a repl
package main

import (
    "flag"
    "fmt"
    "github.com/steelseries/golisp"
)

var (
    runTests bool = false
)

func test() {
    for i := 0; i < flag.NArg(); i = i + 1 {
        fmt.Printf("Loading %s\n", flag.Arg(i))
        _, err := golisp.ProcessFile(flag.Arg(i))
        if err != nil {
            fmt.Printf("Error: %s\n", err)
        }
    }
    golisp.PrintTestResults()
}

func main() {
    flag.BoolVar(&runTests, "t", false, "Whether to run tests and exit.  Defaults to false.")
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
