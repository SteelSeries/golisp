// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the string primitive functions.

package golisp

import (
	"strings"
)

func RegsterStringPrimitives() {
	MakePrimitiveFunction("split", 2, SplitImpl)
	MakePrimitiveFunction("trim", -1, TrimImpl)
}

func SplitImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	theString, err := Eval(First(args), env)
	if err != nil {
		return
	}

	theSeparator, err := Eval(Second(args), env)
	if err != nil {
		return
	}

	pieces := strings.Split(StringValue(theString), StringValue(theSeparator))
	ary := make([]*Data, 0, len(pieces))
	for _, p := range pieces {
		ary = append(ary, StringWithValue(p))
	}
	return ArrayToList(ary), nil
}

func TrimImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	theString, err := Eval(First(args), env)
	if err != nil {
		return
	}

	if Length(args) == 2 {
		theTrimSet, err := Eval(Second(args), env)
		if err != nil {
			return nil, err
		}

		result = StringWithValue(strings.Trim(StringValue(theString), StringValue(theTrimSet)))
	} else {
		result = StringWithValue(strings.TrimSpace(StringValue(theString)))
	}
	return
}
