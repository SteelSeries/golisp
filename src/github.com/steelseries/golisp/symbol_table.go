// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file impliments the symbol table

package golisp

import (
    "container/list"
    "fmt"
)

type SymbolTable struct {
    Frames *list.List
}

var symbolTable *SymbolTable

func (self *SymbolTable) Dump() {
    for frameNumber, frame := 0, self.Frames.Back(); frame != nil; frameNumber, frame = frameNumber+1, frame.Prev() {
        fmt.Printf("Frame %d\n", frameNumber)
        frame.Value.(*SymbolTableFrame).Dump()
    }
}

func findSymbol(name string) (symbol *Data, found bool) {
    for frame := symbolTable.Frames.Front(); frame != nil; frame = frame.Next() {
        binding, present := frame.Value.(*SymbolTableFrame).BindingNamed(name)
        if present {
            return binding.Sym, true
        }
    }
    return
}

func localFrame() *SymbolTableFrame {
    return symbolTable.Frames.Front().Value.(*SymbolTableFrame)
}

func findBindingFor(symbol *Data) (binding *Binding, found bool) {
    name := StringValue(symbol)
    for frame := symbolTable.Frames.Front(); frame != nil; frame = frame.Next() {
        binding, found = frame.Value.(*SymbolTableFrame).BindingNamed(name)
        if found {
            return
        }
    }
    return
}

func findBindingInLocalFrameFor(symbol *Data) (b *Binding, found bool) {
    return localFrame().BindingNamed(StringValue(symbol))
}

func Intern(name string) (sym *Data) {
    sym, found := findSymbol(name)
    if !found {
        sym = SymbolWithName(name)
        BindTo(sym, nil)
        return
    } else {
        return
    }
}

func InternSymbol(sym *Data) {
    _, found := findSymbol(StringValue(sym))
    if !found {
        BindTo(sym, nil)
    }
}

func BindTo(symbol *Data, value *Data) *Data {
    binding, found := findBindingFor(symbol)
    if found {
        binding.Val = value
    } else {
        binding := BindingWithSymbolAndValue(symbol, value)
        localFrame().SetBindingAt(StringValue(symbol), binding)
    }
    return value
}

func BindLocallyTo(symbol *Data, value *Data) *Data {
    binding, found := findBindingInLocalFrameFor(symbol)
    if found {
        binding.Val = value
    } else {
        binding := BindingWithSymbolAndValue(symbol, value)
        localFrame().SetBindingAt(StringValue(symbol), binding)
    }
    return value
}

func ValueOf(symbol *Data) *Data {
    binding, found := findBindingFor(symbol)
    if found {
        return binding.Val
    } else {
        return nil
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
