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
    Body         func(d *Data, env *SymbolTableFrame) (*Data, error)
}

func MakePrimitiveFunction(name string, argCount int, function func(*Data, *SymbolTableFrame) (*Data, error)) {
    f := &PrimitiveFunction{Name: name, NumberOfArgs: argCount, Body: function}
    sym := Global.Intern(name)
    Global.BindTo(sym, PrimitiveWithNameAndFunc(name, f))
}

func (self *PrimitiveFunction) String() string {
    return fmt.Sprintf("<prim: %s, %v>", self.Name, self.Body)
}

func (self *PrimitiveFunction) Apply(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    argCount := Length(args)
    expectedArgs := self.NumberOfArgs
    atLeastOneArg := (expectedArgs == -1) && (argCount > 0)
    exactNumberOfArgs := self.NumberOfArgs == argCount
    var stringArgCount string = fmt.Sprintf("%d", self.NumberOfArgs)
    if self.NumberOfArgs == -1 {
        stringArgCount = "at least 1"
    }
    if !(atLeastOneArg || exactNumberOfArgs) {
        err = errors.New(fmt.Sprintf("Wrong number of args to %s. Expected %s but got %d.\n", self.Name, stringArgCount, argCount))
        return
    }

    return (self.Body)(args, env)
}
