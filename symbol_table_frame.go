// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file implements the symbol table.

package golisp

import (
	"errors"
	"fmt"
)

type SymbolTableFrame struct {
	Parent   *SymbolTableFrame
	Frame    *FrameMap
	Bindings map[string]*Binding
}

var Global *SymbolTableFrame

func (self *SymbolTableFrame) InternalDump(frameNumber int) {
	fmt.Printf("Frame %d\n", frameNumber)
	for _, b := range self.Bindings {
		b.Dump()
	}
	fmt.Printf("\n")
	if self.Parent != nil {
		self.Parent.InternalDump(frameNumber + 1)
	}
}

func (self *SymbolTableFrame) Dump() {
	println()
	self.InternalDump(0)
}

func NewSymbolTableFrameBelow(p *SymbolTableFrame) *SymbolTableFrame {
	var f *FrameMap = nil
	if p != nil {
		f = p.Frame
	}
	return &SymbolTableFrame{Parent: p, Bindings: make(map[string]*Binding), Frame: f}
}

func NewSymbolTableFrameBelowWithFrame(p *SymbolTableFrame, f *FrameMap) *SymbolTableFrame {
	if f == nil {
		f = p.Frame
	}
	return &SymbolTableFrame{Parent: p, Bindings: make(map[string]*Binding, 10), Frame: f}
}

func (self *SymbolTableFrame) HasFrame() bool {
	return self.Frame != nil
}

func (self *SymbolTableFrame) BindingNamed(name string) (b *Binding, present bool) {
	b, present = self.Bindings[name]
	return
}

func (self *SymbolTableFrame) SetBindingAt(name string, b *Binding) {
	self.Bindings[name] = b
}

func (self *SymbolTableFrame) findSymbol(name string) (symbol *Data, found bool) {
	binding, found := self.BindingNamed(name)
	if found {
		return binding.Sym, true
	} else if self.Parent != nil {
		return self.Parent.findSymbol(name)
	} else {
		return nil, false
	}
}

func (self *SymbolTableFrame) findBindingFor(symbol *Data) (binding *Binding, found bool) {
	name := StringValue(symbol)
	binding, found = self.BindingNamed(name)
	if found {
		return
	} else if self.Parent != nil {
		return self.Parent.findBindingFor(symbol)
	} else {
		return nil, false
	}
}

func (self *SymbolTableFrame) Intern(name string) (sym *Data) {
	sym, found := self.findSymbol(name)
	if !found {
		sym = SymbolWithName(name)
		self.BindTo(sym, nil)
		return
	} else {
		return nil
	}
}

func (self *SymbolTableFrame) BindTo(symbol *Data, value *Data) *Data {
	binding, found := self.findBindingFor(symbol)
	if found {
		binding.Val = value
	} else {
		binding := BindingWithSymbolAndValue(symbol, value)
		self.SetBindingAt(StringValue(symbol), binding)
	}
	return value
}

func (self *SymbolTableFrame) SetTo(symbol *Data, value *Data) (result *Data, err error) {
	localBinding, found := self.findBindingInLocalFrameFor(symbol)
	if found {
		localBinding.Val = value
		return value, nil
	}

	naked := StringValue(NakedSymbolFrom(symbol))
	if self.HasFrame() && self.Frame.HasSlot(naked) {
		self.Frame.Set(naked, value)
		return value, nil
	}

	binding, found := self.findBindingFor(symbol)
	if found {
		binding.Val = value
		return value, nil
	}

	return nil, errors.New(fmt.Sprintf("%s is undefined", StringValue(symbol)))
}

func (self *SymbolTableFrame) findBindingInLocalFrameFor(symbol *Data) (b *Binding, found bool) {
	return self.BindingNamed(StringValue(symbol))
}

func (self *SymbolTableFrame) BindLocallyTo(symbol *Data, value *Data) *Data {
	binding, found := self.findBindingInLocalFrameFor(symbol)
	if found {
		binding.Val = value
	} else {
		binding := BindingWithSymbolAndValue(symbol, value)
		self.SetBindingAt(StringValue(symbol), binding)
	}
	return value
}

func (self *SymbolTableFrame) ValueOf(symbol *Data) *Data {
	localBinding, found := self.findBindingInLocalFrameFor(symbol)
	if found {
		return localBinding.Val
	}

	naked := StringValue(NakedSymbolFrom(symbol))
	if self.HasFrame() && self.Frame.HasSlot(naked) {
		return self.Frame.Get(naked)
	}

	binding, found := self.findBindingFor(symbol)
	if found {
		return binding.Val
	} else {
		return nil
	}
}
