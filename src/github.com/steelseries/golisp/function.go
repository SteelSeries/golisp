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
    return &Function{Name: name, Params: params, Body: body, Env: NewSymbolTableFrameBelow(parentEnv)}
}

func (self *Function) String() string {
    return fmt.Sprintf("<func: %s>", self.Name)
}

func (self *Function) makeLocalBindings(args *Data) (err error) {
    if Length(args) != Length(self.Params) {
        return errors.New("Number of args must equal number of params")
    }

    var data *Data
    for p, a := self.Params, args; NotNilP(p); p, a = Cdr(p), Cdr(a) {
        data, err = Eval(Car(a), self.Env)
        if err != nil {
            return
        }
        self.Env.BindLocallyTo(Car(p), data)
    }
    return nil
}

func (self *Function) Apply(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    err = self.makeLocalBindings(args)
    if err != nil {
        return
    }
    for s := self.Body; NotNilP(s); s = Cdr(s) {
        result, err = Eval(Car(s), self.Env)
        if err != nil {
            return
        }
    }
    return
}
