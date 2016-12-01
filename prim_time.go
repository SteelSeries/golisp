// Copyright 2016 Dave Astels.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the Timer/Ticker primitive functions.

package golisp

import (
	"time"
)

func RegisterTimePrimitives() {
	MakePrimitiveFunction("clock", "0", ClockImpl)
	MakePrimitiveFunction("seconds", "0", SecondsImpl)
	MakePrimitiveFunction("date", "0", DateImpl)
	MakePrimitiveFunction("weekday", "0", WeekdayImpl)
}

func ClockImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	h, m, s := time.Now().Local().Clock()
	result = InternalMakeList(IntegerWithValue(int64(h)), IntegerWithValue(int64(m)), IntegerWithValue(int64(s)))
	return
}

func SecondsImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	h, m, s := time.Now().Local().Clock()
	result = IntegerWithValue(int64((h * 3600) + (m * 60) + s))
	return
}

func DateImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	y, m, d := time.Now().Local().Date()
	result = InternalMakeList(IntegerWithValue(int64(y)), IntegerWithValue(int64(m)), IntegerWithValue(int64(d)))
	return
}

func WeekdayImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	result = IntegerWithValue(int64(time.Now().Local().Weekday()))
	return
}
