// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file impliments the symbol table

package golisp

import (
    "container/list"
)

type SymbolTable struct {
    Frames list.List
}

var symbolTable SymbolTable

func init() {
    symbolTable = SymbolTable{*list.New()}
    PushLocalBindings()
}

func findSymbol(name string) (symbol Symbol, found bool) {
    for frame := symbolTable.Frames.Front(); frame != nil; frame = frame.Next() {
        binding, present := frame.Value.(SymbolTableFrame).BindingNamed(name)
        if present {
            return binding.Sym, true
        }
    }
    return
}

func localFrame() SymbolTableFrame {
    return symbolTable.Frames.Front().Value.(SymbolTableFrame)
}

func findBindingFor(symbol Symbol) (binding Binding, found bool) {
    name := symbol.IdentifierValue()
    for frame := symbolTable.Frames.Front(); frame != nil; frame = frame.Next() {
        binding, found = frame.Value.(SymbolTableFrame).BindingNamed(name)
        if found {
            return
        }
    }
    return
}

func findBindingInLocalFrameFor(symbol Symbol) (b Binding, found bool) {
    return localFrame().BindingNamed(symbol.IdentifierValue())
}

func Intern(name string) (sym Symbol) {
    sym, found := findSymbol(name)
    if !found {
        sym = SymbolWithName(name)
        BindTo(sym, &Nil)
        return
    } else {
        return
    }
}

func BindTo(symbol Symbol, value Expression) Expression {
    binding, found := findBindingInLocalFrameFor(symbol)
    if found {
        binding.Val = value
    } else {
        binding := BindingWithSymbolAndValue(symbol, value)
        localFrame().SetBindingAt(symbol.name, binding)
    }
    return value
}

func ValueOf(symbol Symbol) Expression {
    binding, found := findBindingFor(symbol)
    if found {
        return binding.Val
    } else {
        return Nil
    }
}

func PushLocalBindings() {
    symbolTable.Frames.PushFront(NewSymbolTableFrame())
}

func PopLocalBindings() {
    if symbolTable.Frames.Len() > 1 {
        symbolTable.Frames.Remove(symbolTable.Frames.Front())
    }
}
