// Copyright 2013 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file provides a testing framework in Lisp. Added to the global environment in builtins.go
package golisp

import (
    "errors"
    "fmt"
)

var (
    numberOfTests   int      = 0
    numberOfFails   int      = 0
    numberOfPasses  int      = 0
    numberOfErrors  int      = 0
    failureMessages []string = make([]string, 0, 5)
    errorMessages   []string = make([]string, 0, 5)
)

func Describe(args *Data, env *SymbolTableFrame) (d *Data, e error) {
    if !(StringP(Car(args)) || SymbolP(Car(args))) {
        e = errors.New("The describe tag must be a string or symbol")
        return
    }
    fmt.Printf("%s\n", StringValue(Car(args)))

    for clauses := Cdr(args); NotNilP(clauses); clauses = Cdr(clauses) {
        clause := Car(clauses)
        fmt.Printf("   %s - ", String(clause))
        numberOfTests++
        result, err := Eval(clause, env)
        if err != nil {
            numberOfErrors++
            msg := fmt.Sprintf("error: %s", err)
            fmt.Printf("%s\n", msg)
            errorMessages = append(errorMessages, msg)
            break
        } else if BooleanValue(result) {
            numberOfPasses++
            fmt.Printf("ok\n")
        } else {
            numberOfFails++
            value, _ := Eval(Cadr(clause), env)
            msg := fmt.Sprintf("failed: %s is %s", String(Cadr(clause)), String(value))
            fmt.Printf("%s\n", msg)
            failureMessages = append(failureMessages, msg)
        }
    }

    return
}

func dumpMessages(header string, messages []string) {
    if len(messages) > 0 {
        fmt.Printf("%s:\n", header)
        for _, msg := range messages {
            fmt.Printf("  %s\n", msg)
        }
        fmt.Printf("\n")
    }
}

func PrintTestResults() {
    fmt.Printf("\nDone.\n")

    dumpMessages("Errors", errorMessages)
    dumpMessages("Failures", failureMessages)

    fmt.Printf("%d Tests\n", numberOfTests)
    fmt.Printf("%d Passes, %d failures, %d errors\n", numberOfPasses, numberOfFails, numberOfErrors)
}
