// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file implements the frame data type.

package golisp

import (
	"gopkg.in/fatih/set.v0"
	"strings"
)

type FrameMap map[string]*Data

func (self *FrameMap) hasSlotLocally(key string) bool {
	_, ok := (*self)[key]
	return ok
}

func (self *FrameMap) localSlots() []string {
	slots := make([]string, 0, len(*self))
	for k, _ := range *self {
		slots = append(slots, k)
	}
	return slots
}

func isParentKey(key string) bool {
	return strings.HasSuffix(key, "*:")
}

func (self *FrameMap) hasParentSlots() bool {
	for k, _ := range *self {
		if isParentKey(k) {
			return true
		}
	}

	return false
}

func (self *FrameMap) parentSlots() []string {
	slots := make([]string, 0, len(*self))
	for k, _ := range *self {
		if isParentKey(k) {
			slots = append(slots, k)
		}
	}
	return slots
}

func (self *FrameMap) Parents() []*FrameMap {
	parents := make([]*FrameMap, 0, 0)
	for k, v := range *self {
		if isParentKey(k) {
			parents = append(parents, FrameValue(v))
		}
	}
	return parents
}

//------------------------------------------------------------

func (self *FrameMap) hasSlotHelper(key string, v *set.Set) bool {
	if v.Has(self) {
		return false
	}

	v.Add(self)

	if self.hasSlotLocally(key) {
		return true
	}

	if !self.hasParentSlots() {
		return false
	}

	for _, p := range self.Parents() {
		if p.hasSlotHelper(key, v) {
			return true
		}
	}

	return false
}

func (self *FrameMap) HasSlot(key string) bool {
	visited := set.New()
	return self.hasSlotHelper(key, visited)
}

//------------------------------------------------------------

func (self *FrameMap) getHelper(key string, v *set.Set) *Data {
	if v.Has(self) {
		return nil
	}

	v.Add(self)

	val, ok := (*self)[key]
	if ok {
		return val
	}

	for _, p := range self.Parents() {
		val := p.getHelper(key, v)
		if val != nil {
			return val
		}
	}

	return nil
}

func (self *FrameMap) Get(key string) *Data {
	visited := set.New()
	return self.getHelper(key, visited)
}

//------------------------------------------------------------

func (self *FrameMap) Remove(key string) bool {
	if !self.hasSlotLocally(key) {
		return false
	}
	delete(*self, key)
	return true
}

//------------------------------------------------------------

func (self *FrameMap) Set(key string, value *Data) *Data {
	(*self)[key] = value
	return value
}

//------------------------------------------------------------

func (self *FrameMap) Clone() *FrameMap {
	f := make(FrameMap)
	for k, v := range *self {
		f[k] = v
	}
	return &f
}

func (self *FrameMap) Keys() []*Data {
	keys := make([]*Data, 0, len(*self))
	for k, _ := range *self {
		keys = append(keys, SymbolWithName(k))
	}
	return keys
}
