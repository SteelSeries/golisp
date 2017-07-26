// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file implements the frame data type.

package golisp

import (
	"strings"

	"golang.org/x/sync/syncmap"
	"gopkg.in/fatih/set.v0"
)

type FrameMap struct {
	Data syncmap.Map
}

func (self *FrameMap) hasSlotLocally(key string) bool {
	_, ok := self.Data.Load(key)
	return ok
}

func (self *FrameMap) localSlots() []string {
	slots := make([]string, 0)
	self.Data.Range(func(k, v interface{}) bool {
		slots = append(slots, k.(string))
		return true
	})
	return slots
}

func isParentKey(key string) bool {
	return strings.HasSuffix(key, "*:")
}

func (self *FrameMap) hasParentSlots() bool {
	ret := false

	self.Data.Range(func(k, v interface{}) bool {
		if isParentKey(k.(string)) {
			ret = true
			return false
		}
		return true
	})

	return ret
}

func (self *FrameMap) Parents() []*FrameMap {
	parents := make([]*FrameMap, 0)

	self.Data.Range(func(k, v interface{}) bool {
		if isParentKey(k.(string)) && v != nil {
			parents = append(parents, FrameValue(v.(*Data)))
		}
		return true
	})

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
		hasSlot := p.hasSlotHelper(key, v)
		if hasSlot {
			return true
		}
	}

	return false
}

func (self *FrameMap) HasSlot(key string) (ret bool) {
	visited := set.New()
	ret = self.hasSlotHelper(key, visited)
	return
}

//------------------------------------------------------------

func (self *FrameMap) getHelper(key string, v *set.Set) *Data {
	if v.Has(self) {
		return nil
	}

	v.Add(self)

	val, ok := self.Data.Load(key)
	if ok {
		return val.(*Data)
	}

	for _, p := range self.Parents() {
		val := p.getHelper(key, v)
		if val != nil {
			return val
		}
	}

	return nil
}

func (self *FrameMap) Get(key string) (ret *Data) {
	visited := set.New()
	ret = self.getHelper(key, visited)
	return
}

//------------------------------------------------------------

func (self *FrameMap) Remove(key string) bool {
	if !self.hasSlotLocally(key) {
		return false
	}
	self.Data.Delete(key)
	return true
}

//------------------------------------------------------------

func (self *FrameMap) Set(key string, value *Data) *Data {
	self.Data.Store(key, value)
	return value
}

//------------------------------------------------------------

func (self *FrameMap) Clone() *FrameMap {
	f := FrameMap{}
	self.Data.Range(func(k, v interface{}) bool {
		f.Data.Store(k, v)
		return true
	})
	return &f
}

func (self *FrameMap) Keys() []*Data {
	keys := make([]*Data, 0)
	self.Data.Range(func(k, v interface{}) bool {
		keys = append(keys, Intern(k.(string)))
		return true
	})
	return keys
}

func (self *FrameMap) Values() []*Data {
	values := make([]*Data, 0)
	self.Data.Range(func(k, v interface{}) bool {
		values = append(values, v.(*Data))
		return true
	})
	return values
}
