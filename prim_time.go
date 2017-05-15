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
	MakePrimitiveFunction("time-now", "0", timeNowImpl)
	MakePrimitiveFunction("seconds", "0", secondsImpl)
	MakePrimitiveFunction("date-today", "0", dateTodayImpl)
	MakeTypedPrimitiveFunction("date-in-days", "1", dateInDaysImpl, []uint32{IntegerType})
	MakePrimitiveFunction("day-of-week", "0", dayOfWeekImpl)
}

var weekdays = [7]string{"sunday", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday"}

func timeNowImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	h, m, s := time.Now().Local().Clock()
	result = InternalMakeList(IntegerWithValue(int64(h)), IntegerWithValue(int64(m)), IntegerWithValue(int64(s)))
	return
}

func secondsImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	h, m, s := time.Now().Local().Clock()
	result = IntegerWithValue(int64((h * 3600) + (m * 60) + s))
	return
}

func dateTodayImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	y, m, d := time.Now().Local().Date()
	result = InternalMakeList(IntegerWithValue(int64(y)), IntegerWithValue(int64(m)), IntegerWithValue(int64(d)))
	return
}

func dateInDaysImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	y, m, d := time.Now().AddDate(0, 0, int(IntegerValue(First(args)))).Local().Date()
	result = InternalMakeList(IntegerWithValue(int64(y)), IntegerWithValue(int64(m)), IntegerWithValue(int64(d)))
	return
}

func dayOfWeekImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	d := time.Now().Local().Weekday()
	result = Intern(weekdays[d])
	return
}
