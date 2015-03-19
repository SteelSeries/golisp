// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the string primitive functions.

package golisp

import (
	"fmt"
	"strings"
)

func RegsterStringPrimitives() {
	MakePrimitiveFunction("split", 2, SplitImpl)
	MakePrimitiveFunction("trim", -1, TrimImpl)
	MakePrimitiveFunction("string-upcase", 1, UpcaseImpl)
	MakePrimitiveFunction("string-downcase", 1, DowncaseImpl)
}

func SplitImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	theString := First(args)
	if !StringP(theString) {
		err = ProcessError(fmt.Sprintf("trim requires string arguments but was given %s.", String(theString)), env)
		return
	}

	theSeparator := Second(args)
	if !StringP(theSeparator) {
		err = ProcessError(fmt.Sprintf("trim requires string arguments but was given %s.", String(theSeparator)), env)
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
	theString := First(args)

	if Length(args) == 2 {
		theTrimSet := Second(args)
		result = StringWithValue(strings.Trim(StringValue(theString), StringValue(theTrimSet)))
	} else {
		result = StringWithValue(strings.TrimSpace(StringValue(theString)))
	}
	return
}

func UpcaseImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	theString := First(args)
	if !StringP(theString) {
		err = ProcessError(fmt.Sprintf("string-upcase requires a string argument but was given %s.", String(theString)), env)
		return
	}
	return StringWithValue(strings.ToUpper(StringValue(theString))), nil
}

func DowncaseImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	theString := First(args)
	if !StringP(theString) {
		err = ProcessError(fmt.Sprintf("string-downcase requires a string argument but was given %s.", String(theString)), env)
		return
	}
	return StringWithValue(strings.ToLower(StringValue(theString))), nil
}
