// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpreter for embedding in a go program for scripting.
// This file implements a symbol binding.

package golisp

import (
	"fmt"
)

type Binding struct {
	Sym       *Data
	Val       *Data
	Protected bool
}

func (b *Binding) Dump() {
	fmt.Printf("   %s => %s\n", StringValue(b.Sym), String(b.Val))
}

func BindingWithSymbolAndValue(sym *Data, val *Data) *Binding {
	return &Binding{Sym: sym, Val: val}
}

func ProtectedBindingWithSymbolAndValue(sym *Data, val *Data) *Binding {
	return &Binding{Sym: sym, Val: val, Protected: true}
}
