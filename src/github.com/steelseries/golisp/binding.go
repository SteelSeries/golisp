// Copyright 2013 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file impliments a symbol binding

package golisp

import (
    "fmt"
)

type Binding struct {
    Sym *Data
    Val *Data
}

func (self *Binding) Dump() {
    fmt.Printf("   %s => %s\n", StringValue(self.Sym), String(self.Val))
}

func BindingWithSymbolAndValue(sym *Data, val *Data) *Binding {
    return &Binding{sym, val}
}
