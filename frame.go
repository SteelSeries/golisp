// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file implements the frame data type.

package golisp

import (
	"gopkg.in/fatih/set.v0"
	"strings"
	"sync"
)

type FrameMapData map[string]*Data

type FrameMap struct {
	Data  FrameMapData
	Mutex sync.RWMutex
}

func (self *FrameMap) hasSlotLocally(key string) bool {
	self.Mutex.RLock()
	_, ok := self.Data[key]
	self.Mutex.RUnlock()
	return ok
}

func (self *FrameMap) localSlots() []string {
	self.Mutex.RLock()
	slots := make([]string, 0, len(self.Data))
	for k, _ := range self.Data {
		slots = append(slots, k)
	}
	self.Mutex.RUnlock()
	return slots
}

func isParentKey(key string) bool {
	return strings.HasSuffix(key, "*:")
}

func (self *FrameMap) hasParentSlots() bool {
	self.Mutex.RLock()
	for k, _ := range self.Data {
		if isParentKey(k) {
			self.Mutex.RUnlock()
			return true
		}
	}
	self.Mutex.RUnlock()

	return false
}

func (self *FrameMap) parentSlots() []string {
	self.Mutex.RLock()
	slots := make([]string, 0, len(self.Data))
	for k, _ := range self.Data {
		if isParentKey(k) {
			slots = append(slots, k)
		}
	}
	self.Mutex.RUnlock()
	return slots
}

func (self *FrameMap) Parents() []*FrameMap {
	parents := make([]*FrameMap, 0, 0)
	self.Mutex.RLock()
	for k, v := range self.Data {
		if isParentKey(k) && v != nil {
			parents = append(parents, FrameValue(v))
		}
	}
	self.Mutex.RUnlock()
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

	self.Mutex.RLock()
	val, ok := self.Data[key]
	self.Mutex.RUnlock()
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
	self.Mutex.Lock()
	delete(self.Data, key)
	self.Mutex.Unlock()
	return true
}

//------------------------------------------------------------

func (self *FrameMap) Set(key string, value *Data) *Data {
	self.Mutex.Lock()
	self.Data[key] = value
	self.Mutex.Unlock()
	return value
}

//------------------------------------------------------------

func (self *FrameMap) Clone() *FrameMap {
	f := FrameMap{}
	f.Data = make(FrameMapData)
	self.Mutex.RLock()
	for k, v := range self.Data {
		f.Data[k] = v
	}
	self.Mutex.RUnlock()
	return &f
}

func (self *FrameMap) Keys() []*Data {
	self.Mutex.RLock()
	keys := make([]*Data, 0, len(self.Data))
	for k, _ := range self.Data {
		keys = append(keys, Intern(k))
	}
	self.Mutex.RUnlock()
	return keys
}

func (self *FrameMap) Values() []*Data {
	self.Mutex.RLock()
	values := make([]*Data, 0, len(self.Data))
	for _, v := range self.Data {
		values = append(values, v)
	}
	self.Mutex.RUnlock()
	return values
}
