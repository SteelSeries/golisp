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

func toSet(items []string) *set.Set {
	s := set.New()
	for _, i := range items {
		s.Add(i)
	}
	return s
}

func (self *FrameMap) inheritedValueSlots() []string {
	parentFrames := self.Parents()
	inheritedSlots := make([][]string, len(parentFrames))
	for _, p := range parentFrames {
		inheritedSlots = append(inheritedSlots, p.inheritedValueSlots())
	}

	slots := set.New()
	for _, s := range self.localSlots() {
		if !isParentKey(s) {
			slots.Add(s)
		}
	}

	for _, is := range inheritedSlots {
		slots.Merge(toSet(is))
	}

	return set.StringSlice(slots)
}

func (self *FrameMap) HasSlot(key string) bool {
	if self.hasSlotLocally(key) {
		return true
	}

	if !self.hasParentSlots() {
		return false
	}

	for _, p := range self.Parents() {
		if p.HasSlot(key) {
			return true
		}
	}

	return false
}

func (self *FrameMap) Get(key string) *Data {
	v, ok := (*self)[key]
	if ok {
		return v
	}

	for _, p := range self.Parents() {
		v := p.Get(key)
		if v != nil {
			return v
		}
	}

	return nil
}

func (self *FrameMap) Remove(key string) bool {
	if !self.hasSlotLocally(key) {
		return false
	}
	delete(*self, key)
	return true
}

func (self *FrameMap) Set(key string, value *Data) *Data {
	if !self.HasSlot(key) || self.hasSlotLocally(key) {
		(*self)[key] = value
		return value
	}

	for _, p := range self.Parents() {
		v := p.Set(key, value)
		if v != nil {
			return v
		}
	}
	return nil
}

func (self *FrameMap) Clone() *FrameMap {
	f := make(FrameMap)
	for k, v := range *self {
		f[k] = v
	}
	return &f
}
