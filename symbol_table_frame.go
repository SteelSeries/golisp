// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file implements the symbol table.

package golisp

import (
	"container/list"
	"errors"
	"fmt"
)

type SymbolTableFrame struct {
	Parent      *SymbolTableFrame
	Previous    *SymbolTableFrame
	Frame       *FrameMap
	Bindings    map[string]*Binding
	CurrentCode *list.List
}

var Global *SymbolTableFrame

func (self *SymbolTableFrame) Depth() int {
	if self.Previous == nil {
		return 1
	} else {
		return 1 + self.Previous.Depth()
	}
}

func (self *SymbolTableFrame) InternalDump(frameNumber int) {
	fmt.Printf("Frame %d: %s\n", frameNumber, self.CurrentCode.Front().Value)
	for _, b := range self.Bindings {
		if b.Val == nil || TypeOf(b.Val) != PrimitiveType {
			b.Dump()
		}
	}
	fmt.Printf("\n")
	if self.Previous != nil {
		self.Previous.InternalDump(frameNumber + 1)
	}
}

func (self *SymbolTableFrame) Dump() {
	println()
	self.InternalDump(0)
}

func (self *SymbolTableFrame) DumpSingleFrame(frameNumber int) {
	if frameNumber == 0 {
		fmt.Printf("%s\n", self.CurrentCode.Front().Value)
		for _, b := range self.Bindings {
			if b.Val == nil || TypeOf(b.Val) != PrimitiveType {
				b.Dump()
			}
		}
		fmt.Printf("\n")
	} else if self.Previous != nil {
		self.Previous.DumpSingleFrame(frameNumber - 1)
	} else {
		fmt.Printf("Invalid frame selected.\n")
	}
}

func (self *SymbolTableFrame) InternalDumpHeaders(frameNumber int) {
	fmt.Printf("Frame %d: %s\n", frameNumber, self.CurrentCode.Front().Value)
	if self.Previous != nil {
		self.Previous.InternalDumpHeaders(frameNumber + 1)
	}
}

func (self *SymbolTableFrame) DumpHeaders() {
	println()
	self.InternalDumpHeaders(0)
}

func (self *SymbolTableFrame) DumpHeader() {
	fmt.Printf("%s\n", self.CurrentCode.Front().Value)
}

func NewSymbolTableFrameBelow(p *SymbolTableFrame) *SymbolTableFrame {
	var f *FrameMap = nil
	if p != nil {
		f = p.Frame
	}
	return &SymbolTableFrame{Parent: p, Bindings: make(map[string]*Binding), Frame: f, CurrentCode: list.New()}
}

func NewSymbolTableFrameBelowWithFrame(p *SymbolTableFrame, f *FrameMap) *SymbolTableFrame {
	if f == nil {
		f = p.Frame
	}
	return &SymbolTableFrame{Parent: p, Bindings: make(map[string]*Binding, 10), Frame: f, CurrentCode: list.New()}
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
