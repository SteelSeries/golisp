// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file impliments the symbol table

package golisp

type SymbolTableFrame struct {
    Bindings map[string]Binding
}

func NewSymbolTableFrame() SymbolTableFrame {
    return SymbolTableFrame{make(map[string]Binding, 10)}
}

func (self SymbolTableFrame) BindingNamed(name string) (b Binding, present bool) {
    b, present = self.Bindings[name]
    return
}

func (self SymbolTableFrame) SetBindingAt(name string, b Binding) {
    self.Bindings[name] = b
}
