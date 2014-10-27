// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the debugging primitive functions.

package golisp

import ()

func RegisterDebugPrimitives() {
	MakePrimitiveFunction("debug-trace", -1, DebugTraceImpl)
}

func DebugTraceImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if Length(args) == 1 {
		DebugTrace = BooleanValue(Car(args))
	}
	return BooleanWithValue(DebugTrace), nil
}
