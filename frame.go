// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpreter for embedding in a go program for scripting.
// This file implements the frame data type.

package golisp

import (
	"strings"
	"sync"

	"gopkg.in/fatih/set.v0"
)

type FrameMapData map[string]*Data

type FrameMap struct {
	Data  FrameMapData
	Mutex sync.RWMutex
}

func (fm *FrameMap) hasSlotLocally(key string) bool {
	_, ok := fm.Data[key]
	return ok
}

func (fm *FrameMap) localSlots() []string {
	slots := make([]string, 0, len(fm.Data))
	for k := range fm.Data {
		slots = append(slots, k)
	}
	return slots
}

func isParentKey(key string) bool {
	return strings.HasSuffix(key, "*:")
}

func (fm *FrameMap) hasParentSlots() bool {
	for k := range fm.Data {
		if isParentKey(k) {
			return true
		}
	}

	return false
}

func (fm *FrameMap) Parents() []*FrameMap {
	parents := make([]*FrameMap, 0, len(fm.Data))
	for k, v := range fm.Data {
		if isParentKey(k) && v != nil {
			parents = append(parents, FrameValue(v))
		}
	}
	return parents
}

//------------------------------------------------------------

func (fm *FrameMap) hasSlotHelper(key string, v *set.Set) bool {
	if v.Has(fm) {
		return false
	}

	v.Add(fm)

	if fm.hasSlotLocally(key) {
		return true
	}

	if !fm.hasParentSlots() {
		return false
	}

	for _, p := range fm.Parents() {
		p.Mutex.RLock()
		hasSlot := p.hasSlotHelper(key, v)
		p.Mutex.RUnlock()
		if hasSlot {
			return true
		}
	}

	return false
}

func (fm *FrameMap) HasSlot(key string) (ret bool) {
	visited := set.New()
	fm.Mutex.RLock()
	ret = fm.hasSlotHelper(key, visited)
	fm.Mutex.RUnlock()
	return
}

//------------------------------------------------------------

func (fm *FrameMap) getHelper(key string, v *set.Set) *Data {
	if v.Has(fm) {
		return nil
	}

	v.Add(fm)

	val, ok := fm.Data[key]
	if ok {
		return val
	}

	for _, p := range fm.Parents() {
		p.Mutex.RLock()
		val := p.getHelper(key, v)
		p.Mutex.RUnlock()
		if val != nil {
			return val
		}
	}

	return nil
}

func (fm *FrameMap) Get(key string) (ret *Data) {
	visited := set.New()
	fm.Mutex.RLock()
	ret = fm.getHelper(key, visited)
	fm.Mutex.RUnlock()
	return
}

//------------------------------------------------------------

func (fm *FrameMap) Remove(key string) bool {
	fm.Mutex.Lock()
	if !fm.hasSlotLocally(key) {
		fm.Mutex.Unlock()
		return false
	}
	delete(fm.Data, key)
	fm.Mutex.Unlock()
	return true
}

//------------------------------------------------------------

func (fm *FrameMap) Set(key string, value *Data) *Data {
	fm.Mutex.Lock()
	fm.Data[key] = value
	fm.Mutex.Unlock()
	return value
}

//------------------------------------------------------------

func (fm *FrameMap) Clone() *FrameMap {
	f := FrameMap{}
	f.Data = make(FrameMapData)
	fm.Mutex.RLock()
	for k, v := range fm.Data {
		f.Data[k] = v
	}
	fm.Mutex.RUnlock()
	return &f
}

func (fm *FrameMap) Keys() []*Data {
	fm.Mutex.RLock()
	keys := make([]*Data, 0, len(fm.Data))
	for k := range fm.Data {
		keys = append(keys, Intern(k))
	}
	fm.Mutex.RUnlock()
	return keys
}

func (fm *FrameMap) Values() []*Data {
	fm.Mutex.RLock()
	values := make([]*Data, 0, len(fm.Data))
	for _, v := range fm.Data {
		values = append(values, v)
	}
	fm.Mutex.RUnlock()
	return values
}
