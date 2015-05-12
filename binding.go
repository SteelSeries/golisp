// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file implements a symbol binding.

package golisp

type Binding struct {
    Sym *Data
    Val *Data
}

func (self *Binding) Dump() {
    LogPrintf("   %s => %s\n", StringValue(self.Sym), String(self.Val))
}

func BindingWithSymbolAndValue(sym *Data, val *Data) *Binding {
    return &Binding{sym, val}
}
