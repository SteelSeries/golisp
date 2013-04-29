// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file impliments a symbol binding

package golisp

type Binding struct {
    Sym *Data
    Val *Data
}

func BindingWithSymbolAndValue(sym *Data, val *Data) Binding {
    return Binding{sym, val}
}
