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
	Name         string
	Parent       *SymbolTableFrame
	Previous     *SymbolTableFrame
	Frame        *FrameMap
	Bindings     map[string]*Binding
	CurrentCode  *list.List
	IsRestricted bool
}

var Global *SymbolTableFrame
var TopLevelEnvironments map[string]*SymbolTableFrame

var internedSymbols map[string]*Data = make(map[string]*Data, 256)

func Intern(name string) (sym *Data) {
	sym = internedSymbols[name]
	if sym == nil {
		sym = SymbolWithName(name)
		internedSymbols[name] = sym
	}
	return
}

func (self *SymbolTableFrame) Depth() int {
	if self.Previous == nil {
		return 1
	} else {
		return 1 + self.Previous.Depth()
	}
}

func (self *SymbolTableFrame) CurrentCodeString() string {
	if self.CurrentCode.Len() > 0 {
		return self.CurrentCode.Front().Value.(string)
	} else {
		return "Unknown code"
	}
}

func (self *SymbolTableFrame) InternalDump(frameNumber int) {
	fmt.Printf("Frame %d: %s\n", frameNumber, self.CurrentCodeString())
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
		fmt.Printf("%s\n", self.CurrentCodeString())
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
	fmt.Printf("Frame %d: %s\n", frameNumber, self.CurrentCodeString())
	if self.Previous != nil {
		self.Previous.InternalDumpHeaders(frameNumber + 1)
	}
}

func (self *SymbolTableFrame) DumpHeaders() {
	println()
	self.InternalDumpHeaders(0)
}

func (self *SymbolTableFrame) DumpHeader() {
	fmt.Printf("%s\n", self.CurrentCodeString())
}

func NewSymbolTableFrameBelow(p *SymbolTableFrame, name string) *SymbolTableFrame {
	var f *FrameMap = nil
	if p != nil {
		f = p.Frame
	}
	restricted := p != nil && p.IsRestricted
	env := &SymbolTableFrame{Name: name, Parent: p, Bindings: make(map[string]*Binding), Frame: f, CurrentCode: list.New(), IsRestricted: restricted}
	if p == nil || p == Global {
		TopLevelEnvironments[name] = env
	}
	return env
}

func NewSymbolTableFrameBelowWithFrame(p *SymbolTableFrame, f *FrameMap, name string) *SymbolTableFrame {
	if f == nil {
		f = p.Frame
	}
	restricted := p != nil && p.IsRestricted
	env := &SymbolTableFrame{Name: name, Parent: p, Bindings: make(map[string]*Binding, 10), Frame: f, CurrentCode: list.New(), IsRestricted: restricted}
	if p == nil || p == Global {
		TopLevelEnvironments[name] = env
	}
	return env
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

func (self *SymbolTableFrame) FindBindingFor(symbol *Data) (binding *Binding, found bool) {
	name := StringValue(symbol)
	binding, found = self.BindingNamed(name)
	if found {
		return
	} else if self.Parent != nil {
		return self.Parent.FindBindingFor(symbol)
	} else {
		return nil, false
	}
}

func (self *SymbolTableFrame) Intern(name string) (sym *Data) {
	sym = Intern(name)
	self.BindTo(sym, nil)
	return
}

func (self *SymbolTableFrame) BindTo(symbol *Data, value *Data) *Data {
	binding, found := self.FindBindingFor(symbol)
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

	binding, found := self.FindBindingFor(symbol)
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

func (self *SymbolTableFrame) ValueOfWithFunctionSlotCheck(symbol *Data, needFunction bool) *Data {
	localBinding, found := self.findBindingInLocalFrameFor(symbol)
	if found {
		return localBinding.Val
	}

	if self.HasFrame() {
		f := self.Frame
		naked := StringValue(NakedSymbolFrom(symbol))
		if f.HasSlot(naked) {
			slotValue := f.Get(naked)
			if !needFunction || FunctionP(slotValue) {
				return slotValue
			}
		}
	}

	binding, found := self.FindBindingFor(symbol)
	if found {
		return binding.Val
	} else {
		return EmptyCons()
	}
}

func (self *SymbolTableFrame) ValueOf(symbol *Data) *Data {
	return self.ValueOfWithFunctionSlotCheck(symbol, false)
}
