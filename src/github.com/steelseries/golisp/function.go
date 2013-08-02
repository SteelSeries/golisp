// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file inmpliments user drfined functions.
package golisp

import (
    "errors"
    "fmt"
)

type Function struct {
    Name   string
    Params *Data
    Body   *Data
    Env    *SymbolTableFrame
}

func MakeFunction(name string, params *Data, body *Data, parentEnv *SymbolTableFrame) *Function {
    return &Function{Name: name, Params: params, Body: body, Env: parentEnv}
}

func (self *Function) String() string {
    return fmt.Sprintf("<func: %s>", self.Name)
}

func (self *Function) makeLocalBindings(args *Data, argEnv *SymbolTableFrame, localEnv *SymbolTableFrame, eval bool) (err error) {
    if Length(args) != Length(self.Params) {
        return errors.New("Number of args must equal number of params")
    }

    var data *Data
    for p, a := self.Params, args; NotNilP(p); p, a = Cdr(p), Cdr(a) {
        if eval {
            data, err = Eval(Car(a), argEnv)
            if err != nil {
                return
            }
        } else {
            data = Car(a)
        }
        localEnv.BindLocallyTo(Car(p), data)
    }
    return nil
}

func (self *Function) internalApply(args *Data, argEnv *SymbolTableFrame, eval bool) (result *Data, err error) {
    localEnv := NewSymbolTableFrameBelow(self.Env)
    err = self.makeLocalBindings(args, argEnv, localEnv, eval)
    if err != nil {
        return
    }
    for s := self.Body; NotNilP(s); s = Cdr(s) {
        result, err = Eval(Car(s), localEnv)
        if err != nil {
            return
        }
    }
    return
}

func (self *Function) Apply(args *Data, argEnv *SymbolTableFrame) (result *Data, err error) {
    return self.internalApply(args, argEnv, true)
}

func (self *Function) ApplyWithoutEval(args *Data, argEnv *SymbolTableFrame) (result *Data, err error) {
    return self.internalApply(args, argEnv, false)
}
