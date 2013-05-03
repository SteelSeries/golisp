// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file inmpliments primitive functions.
package golisp

import (
    "errors"
    "fmt"
)

type PrimitiveFunction struct {
    Name         string
    NumberOfArgs int
    Body         func(d *Data) (*Data, error)
}

func MakePrimitiveFunction(name string, argCount int, function func(d *Data) (*Data, error)) {
    f := &PrimitiveFunction{Name: name, NumberOfArgs: argCount, Body: function}
    sym := Intern(name)
    BindTo(sym, PrimitiveWithNameAndFunc(name, f))
}

func (self *PrimitiveFunction) String() string {
    return fmt.Sprintf("<prim: %s, %v>", self.Name, self.Body)
}

func (self *PrimitiveFunction) Apply(args *Data) (result *Data, err error) {
    argCount := Length(args)
    expectedArgs := self.NumberOfArgs
    atLeastOneArg := (expectedArgs == -1) && (argCount > 0)
    exactNumberOfArgs := self.NumberOfArgs == Length(args)
    if !(atLeastOneArg || exactNumberOfArgs) {
        err = errors.New(fmt.Sprintf("Wrong number of args to %s. Expected %d but got %d.\n", self.Name, expectedArgs, argCount))
        return
    }

    return (self.Body)(args)
}
