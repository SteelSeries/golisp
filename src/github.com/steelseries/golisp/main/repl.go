// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file provides a repl
package main

import (
    "bufio"
    "fmt"
    "github.com/steelseries/golisp"
    "os"
    "strings"
)

func main() {
    for true {
        in := bufio.NewReader(os.Stdin)
        input := ""

        for true {
            fmt.Printf(">")
            input, err := in.ReadString('\n')
            if err != nil {
                panic(err)
            }
            input = strings.TrimRight(input, "\r\n")
            code, err := golisp.Parse(input)
            if err != nil {
                fmt.Printf("Error: %s\n", err)
            } else {
                d, err := golisp.Eval(code)
                if err != nil {
                    fmt.Printf("Error in evaluation: %s\n", err)
                } else {
                    fmt.Printf("==> %s\n", golisp.String(d))
                }
            }
        }
    }
}
