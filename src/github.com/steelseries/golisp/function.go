// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file inmpliments user drfined functions.
package golisp

import (
    "fmt"
)

type Function struct {
    Name   string
    Params *Data
    Body   *Data
}

func MakeFunction(name string, args *Data, body *Data) *Function {
    return &Function{Name: name, Params: args, Body: body}
}

func (self *Function) String() string {
    return fmt.Sprintf("<func: %s>", self.Name)
}

// func makeLocalBindings(args *Data) {
//     argPairs := pairsFrom(self.Params, args)
//     PushLocalBindings()
//     for c := argPairs; NotNilP(c); c = Cdr(c) {
//         BindTo(Caar(c), Cdar(c))
//     }
// }

func (self *Function) Apply(args *Data) (result *Data, err error) {
    // makeLocalBindings(args)
    // for s := self.Body; NotNilP(s); s = Cdr(s) {
    //     result, err = Eval(s)
    //     if err != nil {
    //         return
    //     }
    // }
    // PopLocalBindings()
    return
}
