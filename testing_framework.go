// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file provides a testing framework in Lisp.

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

var VerboseTests = true

func RegisterTestingPrimitives() {
	MakeSpecialForm("describe", "*", DescribeImpl)
	MakeSpecialForm("assert-true", "1", AssertTrueImpl)
	MakeSpecialForm("assert-false", "1", AssertFalseImpl)
	MakeSpecialForm("assert-eq", "2", AssertEqImpl)
	MakeSpecialForm("assert-neq", "2", AssertNeqImpl)
	MakeSpecialForm("assert-nil", "1", AssertNilImpl)
	MakeSpecialForm("assert-not-nil", "1", AssertNotNilImpl)
	MakeSpecialForm("assert-error", "1", AssertErrorImpl)
}

func logPass() {
	numberOfTests++
	numberOfPasses++
	if VerboseTests {
		fmt.Printf("ok\n")
	}
}

func logFailure(clause *Data, msg string) {
	numberOfTests++
	numberOfFails++
	if VerboseTests {
		fmt.Printf("failed: %s\n", msg)
	}
	failureMessages = append(failureMessages, fmt.Sprintf("%s - %s", String(clause), msg))
}

func logError(clause *Data, err error) {
	numberOfTests++
	numberOfErrors++
	msg := fmt.Sprintf("error: %s", err)
	if VerboseTests {
		fmt.Printf("%s\n", msg)
	}
	errorMessages = append(errorMessages, fmt.Sprintf("%s - %s", String(clause), msg))
}

func DescribeImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if !(StringP(Car(args)) || SymbolP(Car(args))) {
		err = errors.New("The describe tag must be a string or symbol")
		return
	}
	if VerboseTests {
		fmt.Printf("%s\n", StringValue(Car(args)))
	}

	for clauses := Cdr(args); NotNilP(clauses); clauses = Cdr(clauses) {
		clause := Car(clauses)
		if VerboseTests {
			fmt.Printf("   %s - ", String(clause))
		}
		_, err = Eval(clause, env)
		if err != nil {
			logError(Cadr(clause), err)
			break
		}
	}

	return
}

func AssertTrueImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	d, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if BooleanValue(d) {
		logPass()
	} else {
		logFailure(Car(args), ("expected true, was false"))
	}
	return
}

func AssertFalseImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	d, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !BooleanValue(d) {
		logPass()
	} else {
		logFailure(Car(args), "expected false, was true")
	}
	return
}

func AssertEqImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	actual, err := Eval(Car(args), env)
	if err != nil {
		return
	}

	expected, err := Eval(Cadr(args), env)
	if err != nil {
		return
	}

	if IsEqual(actual, expected) {
		logPass()
	} else {
		logFailure(Car(args), fmt.Sprintf("expected %s, was %s", String(expected), String(actual)))
	}
	return
}

func AssertNeqImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	actual, err := Eval(Car(args), env)
	if err != nil {
		return
	}

	expected, err := Eval(Cadr(args), env)
	if err != nil {
		return
	}

	if !IsEqual(actual, expected) {
		logPass()
	} else {
		logFailure(Car(args), fmt.Sprintf("expected not %s, but it was", String(expected)))
	}
	return
}

func AssertNilImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	d, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if NilP(d) {
		logPass()
	} else {
		logFailure(Car(args), fmt.Sprintf("expected nil, was %s", String(d)))
	}
	return
}

func AssertNotNilImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	d, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !NilP(d) {
		logPass()
	} else {
		logFailure(Car(args), "expected non-nil")
	}
	return
}

func AssertErrorImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	_, e := Eval(Car(args), env)
	if e != nil {
		logPass()
	} else {
		logFailure(Car(args), "didn't raise an error")
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
