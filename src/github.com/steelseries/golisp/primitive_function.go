// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file implements primitive functions.

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
    anyNumberArgs := expectedArgs == -1
    exactNumberOfArgs := self.NumberOfArgs == argCount
    var stringArgCount string = fmt.Sprintf("%d", self.NumberOfArgs)
    if !(anyNumberArgs || exactNumberOfArgs) {
        err = errors.New(fmt.Sprintf("Wrong number of args to %s. Expected %s but got %d.\n", self.Name, stringArgCount, argCount))
        return
    }

    return (self.Body)(args, env)
}

func (self *PrimitiveFunction) ApplyWithoutEval(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    return self.Apply(args, env)
}
