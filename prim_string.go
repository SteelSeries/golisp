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
	MakePrimitiveFunction("split", "2", SplitImpl)
	MakePrimitiveFunction("trim", "1|2", TrimImpl)
	MakePrimitiveFunction("string-upcase", "1", StringUpcaseImpl)
	MakePrimitiveFunction("string-upcase!", "1", StringUpcaseBangImpl)
	MakePrimitiveFunction("string-downcase", "1", StringDowncaseImpl)
	MakePrimitiveFunction("string-downcase!", "1", StringDowncaseBangImpl)
	MakePrimitiveFunction("string-capitalize", "1", StringCapitalizeImpl)
	MakePrimitiveFunction("string-capitalize!", "1", StringCapitalizeBangImpl)
	MakePrimitiveFunction("string-length", "1", StringLengthImpl)
	MakePrimitiveFunction("string-null?", "1", StringNullImpl)
}

func SplitImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	theString := Car(args)
	if !StringP(theString) {
		err = ProcessError(fmt.Sprintf("trim requires string arguments but was given %s.", String(theString)), env)
		return
	}

	theSeparator := Cadr(args)
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
	theString := Car(args)

	if Length(args) == 2 {
		theTrimSet := Cadr(args)
		result = StringWithValue(strings.Trim(StringValue(theString), StringValue(theTrimSet)))
	} else {
		result = StringWithValue(strings.TrimSpace(StringValue(theString)))
	}
	return
}

func StringUpcaseImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	theString := Car(args)
	if !StringP(theString) {
		err = ProcessError(fmt.Sprintf("string-upcase requires a string argument but was given %s.", String(theString)), env)
		return
	}
	return StringWithValue(strings.ToUpper(StringValue(theString))), nil
}

func StringUpcaseBangImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	theString := Car(args)
	if !StringP(theString) {
		err = ProcessError(fmt.Sprintf("string-upcase requires a string argument but was given %s.", String(theString)), env)
		return
	}
	return SetStringValue(theString, strings.ToUpper(StringValue(theString))), nil
}

func StringDowncaseImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	theString := Car(args)
	if !StringP(theString) {
		err = ProcessError(fmt.Sprintf("string-downcase requires a string argument but was given %s.", String(theString)), env)
		return
	}
	return StringWithValue(strings.ToLower(StringValue(theString))), nil
}

func StringDowncaseBangImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	theString := Car(args)
	if !StringP(theString) {
		err = ProcessError(fmt.Sprintf("string-downcase requires a string argument but was given %s.", String(theString)), env)
		return
	}
	return SetStringValue(theString, strings.ToLower(StringValue(theString))), nil
}

func capitalize(s string) string {
	firstChar := s[0:1]
	remainingChars := s[1:]
	parts := make([]string, 0, 2)
	parts = append(parts, strings.ToUpper(firstChar))
	parts = append(parts, strings.ToLower(remainingChars))
	return strings.Join(parts, "")
}

func StringCapitalizeImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	theString := Car(args)
	
	if !StringP(theString) {
		err = ProcessError(fmt.Sprintf("string-capitalize requires a string argument but was given %s.", String(theString)), env)
		return
	}
	return StringWithValue(capitalize(StringValue(theString))), nil
}

func StringCapitalizeBangImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	theString := Car(args)
	
	if !StringP(theString) {
		err = ProcessError(fmt.Sprintf("string-capitalize requires a string argument but was given %s.", String(theString)), env)
		return
	}
	return SetStringValue(theString, capitalize(StringValue(theString))), nil
}


func StringLengthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	theString := Car(args)
	
	if !StringP(theString) {
		err = ProcessError(fmt.Sprintf("string-capitalize requires a string argument but was given %s.", String(theString)), env)
		return
	}
	return IntegerWithValue(int64(len(StringValue(theString)))), nil
}

func StringNullImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	theString := Car(args)
	
	if !StringP(theString) {
		err = ProcessError(fmt.Sprintf("string-capitalize requires a string argument but was given %s.", String(theString)), env)
		return
	}
	return BooleanWithValue(len(StringValue(theString)) == 0), nil
}
