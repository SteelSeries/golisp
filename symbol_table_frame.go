// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpreter for embedding in a go program for scripting.
// This file implements the symbol table.

package golisp

import (
	"container/list"
	"errors"
	"fmt"
	"sync"
	"sync/atomic"
)

type SymbolTableFrame struct {
	Name         string
	Parent       *SymbolTableFrame
	Previous     *SymbolTableFrame
	Frame        *FrameMap
	Bindings     map[string]*Binding
	Mutex        sync.RWMutex
	CurrentCode  *list.List
	IsRestricted bool
}

type symbolsTable struct {
	Symbols map[string]*Data
	Mutex   sync.RWMutex
}

type environmentsTable struct {
	Environments map[string]*SymbolTableFrame
	Mutex        sync.RWMutex
}

var Global *SymbolTableFrame
var TopLevelEnvironments = environmentsTable{make(map[string]*SymbolTableFrame, 5), sync.RWMutex{}}

var internedSymbols = symbolsTable{make(map[string]*Data, 256), sync.RWMutex{}}

func Intern(name string) (sym *Data) {
	internedSymbols.Mutex.RLock()
	sym = internedSymbols.Symbols[name]
	internedSymbols.Mutex.RUnlock()
	if sym == nil {
		internedSymbols.Mutex.Lock()
		sym = SymbolWithName(name)
		internedSymbols.Symbols[name] = sym
		internedSymbols.Mutex.Unlock()
	}
	return
}

func (stf *SymbolTableFrame) Depth() int {
	if stf.Previous == nil {
		return 1
	} else {
		return 1 + stf.Previous.Depth()
	}
}

func (stf *SymbolTableFrame) CurrentCodeString() string {
	if stf.CurrentCode.Len() > 0 {
		return stf.CurrentCode.Front().Value.(string)
	} else {
		return "Unknown code"
	}
}

func (stf *SymbolTableFrame) InternalDump(frameNumber int) {
	fmt.Printf("Frame %d: %s\n", frameNumber, stf.CurrentCodeString())
	stf.Mutex.RLock()
	defer stf.Mutex.RUnlock()
	for _, b := range stf.Bindings {
		if b.Val == nil || TypeOf(b.Val) != PrimitiveType {
			b.Dump()
		}
	}
	fmt.Printf("\n")
	if stf.Previous != nil {
		stf.Previous.InternalDump(frameNumber + 1)
	}
}

func (stf *SymbolTableFrame) Dump() {
	println()
	stf.InternalDump(0)
}

func (stf *SymbolTableFrame) DumpSingleFrame(frameNumber int) {
	if frameNumber == 0 {
		fmt.Printf("%s\n", stf.CurrentCodeString())
		stf.Mutex.RLock()
		defer stf.Mutex.RUnlock()
		for _, b := range stf.Bindings {
			if b.Val == nil || TypeOf(b.Val) != PrimitiveType {
				b.Dump()
			}
		}
		fmt.Printf("\n")
	} else if stf.Previous != nil {
		stf.Previous.DumpSingleFrame(frameNumber - 1)
	} else {
		fmt.Printf("Invalid frame selected.\n")
	}
}

func (stf *SymbolTableFrame) InternalDumpHeaders(frameNumber int) {
	fmt.Printf("Frame %d: %s\n", frameNumber, stf.CurrentCodeString())
	if stf.Previous != nil {
		stf.Previous.InternalDumpHeaders(frameNumber + 1)
	}
}

func (stf *SymbolTableFrame) DumpHeaders() {
	println()
	stf.InternalDumpHeaders(0)
}

func (stf *SymbolTableFrame) DumpHeader() {
	fmt.Printf("%s\n", stf.CurrentCodeString())
}

func NewSymbolTableFrameBelow(p *SymbolTableFrame, name string) *SymbolTableFrame {
	var f *FrameMap = nil
	if p != nil {
		f = p.Frame
	}
	restricted := p != nil && p.IsRestricted
	env := &SymbolTableFrame{Name: name, Parent: p, Bindings: make(map[string]*Binding), Frame: f, CurrentCode: list.New(), IsRestricted: restricted}
	if p == nil || p == Global {
		TopLevelEnvironments.Mutex.Lock()
		TopLevelEnvironments.Environments[name] = env
		TopLevelEnvironments.Mutex.Unlock()
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
		TopLevelEnvironments.Mutex.Lock()
		TopLevelEnvironments.Environments[name] = env
		TopLevelEnvironments.Mutex.Unlock()
	}
	return env
}

func (stf *SymbolTableFrame) HasFrame() bool {
	return stf.Frame != nil
}

func (stf *SymbolTableFrame) BindingNamed(name string) (b *Binding, present bool) {
	stf.Mutex.RLock()
	b, present = stf.Bindings[name]
	stf.Mutex.RUnlock()
	return
}

func (stf *SymbolTableFrame) SetBindingAt(name string, b *Binding) {
	stf.Mutex.Lock()
	stf.Bindings[name] = b
	stf.Mutex.Unlock()
}

func (stf *SymbolTableFrame) DeleteBinding(name string) {
	stf.Mutex.Lock()
	delete(stf.Bindings, name)
	stf.Mutex.Unlock()
}

func (stf *SymbolTableFrame) findSymbol(name string) (symbol *Data, found bool) {
	binding, found := stf.BindingNamed(name)
	if found {
		return binding.Sym, true
	} else if stf.Parent != nil {
		return stf.Parent.findSymbol(name)
	} else {
		return nil, false
	}
}

func (stf *SymbolTableFrame) FindBindingFor(symbol *Data) (binding *Binding, found bool) {
	name := StringValue(symbol)
	binding, found = stf.BindingNamed(name)
	if found {
		return
	} else if stf.Parent != nil {
		return stf.Parent.FindBindingFor(symbol)
	} else {
		return nil, false
	}
}

func (stf *SymbolTableFrame) BindTo(symbol *Data, value *Data) (*Data, error) {
	binding, found := stf.FindBindingFor(symbol)
	if found {
		if binding.Protected {
			return nil, fmt.Errorf("%s is a protected binding", StringValue(symbol))
		}
		binding.Val = value
	} else {
		binding = BindingWithSymbolAndValue(symbol, value)
		stf.SetBindingAt(StringValue(symbol), binding)
	}
	return binding.Val, nil
}

func (stf *SymbolTableFrame) BindToProtected(symbol *Data, value *Data) *Data {
	binding, found := stf.FindBindingFor(symbol)
	if found {
		binding.Val = value
		binding.Protected = true
	} else {
		binding = ProtectedBindingWithSymbolAndValue(symbol, value)
		stf.SetBindingAt(StringValue(symbol), binding)
	}
	return binding.Val
}

func (stf *SymbolTableFrame) SetTo(symbol *Data, value *Data) (result *Data, err error) {
	localBinding, found := stf.findBindingInLocalFrameFor(symbol)
	if found {
		if localBinding.Protected {
			return nil, fmt.Errorf("%s is a protected binding", StringValue(symbol))
		} else {
			localBinding.Val = value
			return value, nil
		}
	}

	naked := StringValue(NakedSymbolFrom(symbol))
	if stf.HasFrame() && stf.Frame.HasSlot(naked) {
		stf.Frame.Set(naked, value)
		return value, nil
	}

	binding, found := stf.FindBindingFor(symbol)
	if found {
		if binding.Protected {
			return nil, fmt.Errorf("%s is a protected binding", StringValue(symbol))
		} else {
			binding.Val = value
			return value, nil
		}
	}

	return nil, errors.New(fmt.Sprintf("%s is undefined", StringValue(symbol)))
}

func (stf *SymbolTableFrame) findBindingInLocalFrameFor(symbol *Data) (b *Binding, found bool) {
	return stf.BindingNamed(StringValue(symbol))
}

func (stf *SymbolTableFrame) BindLocallyTo(symbol *Data, value *Data) (*Data, error) {
	binding, found := stf.findBindingInLocalFrameFor(symbol)
	if found {
		if binding.Protected {
			return nil, fmt.Errorf("%s is a protected binding", StringValue(symbol))
		}
		binding.Val = value
	} else {
		binding = BindingWithSymbolAndValue(symbol, value)
		stf.SetBindingAt(StringValue(symbol), binding)
	}
	return binding.Val, nil
}

func (stf *SymbolTableFrame) ValueOfWithFunctionSlotCheck(symbol *Data, needFunction bool) *Data {
	localBinding, found := stf.findBindingInLocalFrameFor(symbol)
	if found {
		if FunctionP(localBinding.Val) {
			atomic.StoreInt32(&FunctionValue(localBinding.Val).SlotFunction, 1)
		}
		return localBinding.Val
	}

	if stf.HasFrame() {
		f := stf.Frame
		naked := StringValue(NakedSymbolFrom(symbol))
		if f.HasSlot(naked) {
			slotValue := f.Get(naked)
			if !needFunction {
				return slotValue
			}
			if FunctionP(slotValue) {
				atomic.StoreInt32(&FunctionValue(slotValue).SlotFunction, 1)
				return slotValue
			}
		}
	}

	binding, found := stf.FindBindingFor(symbol)
	if found {
		if FunctionP(binding.Val) {
			atomic.StoreInt32(&FunctionValue(binding.Val).SlotFunction, 0)
		}
		return binding.Val
	} else {
		return EmptyCons()
	}
}

func (stf *SymbolTableFrame) ValueOf(symbol *Data) *Data {
	return stf.ValueOfWithFunctionSlotCheck(symbol, false)
}
