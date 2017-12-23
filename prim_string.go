// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpreter for embedding in a go program for scripting.
// This file contains the string primitive functions.

package golisp

import (
	"fmt"
	"strings"
)

const (
	TrimLeft  = iota
	TrimBoth  = iota
	TrimRight = iota
)

func RegisterStringPrimitives() {
	MakePrimitiveFunction("string-split", "2", StringSplitImpl)
	MakePrimitiveFunction("string-join", "1|2", StringJoinImpl)
	MakePrimitiveFunction("string-trim", "1|2", StringTrimImpl)
	MakePrimitiveFunction("string-trim-left", "1|2", StringTrimLeftImpl)
	MakePrimitiveFunction("string-trim-right", "1|2", StringTrimRightImpl)
	MakePrimitiveFunction("string-upcase", "1", StringUpcaseImpl)
	MakePrimitiveFunction("string-upcase!", "1", StringUpcaseBangImpl)
	MakePrimitiveFunction("string-downcase", "1", StringDowncaseImpl)
	MakePrimitiveFunction("string-downcase!", "1", StringDowncaseBangImpl)
	MakePrimitiveFunction("string-capitalize", "1", StringCapitalizeImpl)
	MakePrimitiveFunction("string-capitalize!", "1", StringCapitalizeBangImpl)
	MakePrimitiveFunction("string-length", "1", StringLengthImpl)
	MakePrimitiveFunction("string-null?", "1", StringNullImpl)
	MakePrimitiveFunction("substring", "3", SubstringImpl)
	MakePrimitiveFunction("substring?", "2", SubstringpImpl)
	MakePrimitiveFunction("string-prefix?", "2", StringPrefixpImpl)
	MakePrimitiveFunction("string-suffix?", "2", StringSuffixpImpl)

	MakePrimitiveFunction("string=?", "2", StringEqualImpl)
	MakePrimitiveFunction("string-ci=?", "2", StringEqualCiImpl)
	MakePrimitiveFunction("string<?", "2", StringLessThanImpl)
	MakePrimitiveFunction("string-ci<?", "2", StringLessThanCiImpl)
	MakePrimitiveFunction("string>?", "2", StringGreaterThanImpl)
	MakePrimitiveFunction("string-ci>?", "2", StringGreaterThanCiImpl)
	MakePrimitiveFunction("string<=?", "2", StringLessThanEqualImpl)
	MakePrimitiveFunction("string-ci<=?", "2", StringLessThanEqualCiImpl)
	MakePrimitiveFunction("string>=?", "2", StringGreaterThanEqualImpl)
	MakePrimitiveFunction("string-ci>=?", "2", StringGreaterThanEqualCiImpl)

	MakePrimitiveFunction("parse", "1", ParseImpl)
}

func StringSplitImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	theString := Car(args)
	if !StringP(theString) {
		err = ProcessError(fmt.Sprintf("trim requires a string but was given %s.", String(theString)), env)
		return
	}

	theSeparator := Cadr(args)
	if !StringP(theSeparator) {
		err = ProcessError(fmt.Sprintf("string-split requires a string separater but was given %s.", String(theSeparator)), env)
		return
	}

	pieces := strings.Split(StringValue(theString), StringValue(theSeparator))
	ary := make([]*Data, 0, len(pieces))
	for _, p := range pieces {
		ary = append(ary, StringWithValue(p))
	}
	return ArrayToList(ary), nil
}

func StringJoinImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	theStrings := Car(args)
	if !ListP(theStrings) {
		err = ProcessError(fmt.Sprintf("string-join requires a list of strings to be joined but was given %s.", String(theStrings)), env)
		return
	}

	theSeparator := Cadr(args)
	separator := ""
	if !NilP(theSeparator) {
		if !StringP(theSeparator) {
			err = ProcessError(fmt.Sprintf("string-join requires a string separater but was given %s.", String(theSeparator)), env)
			return
		}
		separator = StringValue(theSeparator)
	}

	resultStrings := make([]string, 0, Length(theStrings))
	for c := theStrings; NotNilP(c); c = Cdr(c) {
		val := Car(c)
		if !StringP(val) {
			err = ProcessError(fmt.Sprintf("string-join requires a list of strings but %s was in the list.", String(val)), env)
			return
		}
		resultStrings = append(resultStrings, StringValue(val))
	}
	joinedString := strings.Join(resultStrings, separator)
	return StringWithValue(joinedString), nil
}

func doTrim(lrb int, args *Data, env *SymbolTableFrame) (result *Data, err error) {
	theString := Car(args)

	if !StringP(theString) {
		err = ProcessError(fmt.Sprintf("string-trim requires a string but was given %s.", String(theString)), env)
		return
	}

	var trimset string
	if Length(args) == 2 {
		theTrimSet := Cadr(args)
		if !StringP(theTrimSet) {
			err = ProcessError(fmt.Sprintf("string-trim requires a string set of trim characters but was given %s.", String(theTrimSet)), env)
			return
		}

		trimset = StringValue(theTrimSet)
	} else {
		trimset = " \t\r\n\v\f"
	}
	switch lrb {
	case TrimLeft:
		return StringWithValue(strings.TrimLeft(StringValue(theString), trimset)), nil
	case TrimBoth:
		return StringWithValue(strings.Trim(StringValue(theString), trimset)), nil
	case TrimRight:
		return StringWithValue(strings.TrimRight(StringValue(theString), trimset)), nil
	default:
		return theString, nil
	}
}

func StringTrimImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return doTrim(TrimBoth, args, env)
}

func StringTrimLeftImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return doTrim(TrimLeft, args, env)
}

func StringTrimRightImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return doTrim(TrimRight, args, env)
}

func StringUpcaseImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	theString := Car(args)
	if !StringP(theString) {
		err = ProcessError(fmt.Sprintf("string-upcase requires a string but was given %s.", String(theString)), env)
		return
	}
	return StringWithValue(strings.ToUpper(StringValue(theString))), nil
}

func StringUpcaseBangImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	theString := Car(args)
	if !StringP(theString) {
		err = ProcessError(fmt.Sprintf("string-upcase! requires a string but was given %s.", String(theString)), env)
		return
	}
	return SetStringValue(theString, strings.ToUpper(StringValue(theString))), nil
}

func StringDowncaseImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	theString := Car(args)
	if !StringP(theString) {
		err = ProcessError(fmt.Sprintf("string-downcase requires a string but was given %s.", String(theString)), env)
		return
	}
	return StringWithValue(strings.ToLower(StringValue(theString))), nil
}

func StringDowncaseBangImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	theString := Car(args)
	if !StringP(theString) {
		err = ProcessError(fmt.Sprintf("string-downcase! requires a string but was given %s.", String(theString)), env)
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
		err = ProcessError(fmt.Sprintf("string-capitalize requires a string but was given %s.", String(theString)), env)
		return
	}
	return StringWithValue(capitalize(StringValue(theString))), nil
}

func StringCapitalizeBangImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	theString := Car(args)

	if !StringP(theString) {
		err = ProcessError(fmt.Sprintf("string-capitalize! requires a string but was given %s.", String(theString)), env)
		return
	}
	return SetStringValue(theString, capitalize(StringValue(theString))), nil
}

func StringLengthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	theString := Car(args)

	if !StringP(theString) {
		err = ProcessError(fmt.Sprintf("string-length requires a string but was given %s.", String(theString)), env)
		return
	}
	return IntegerWithValue(int64(len(StringValue(theString)))), nil
}

func StringNullImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	theString := Car(args)

	if !StringP(theString) {
		err = ProcessError(fmt.Sprintf("string-null? requires a string but was given %s.", String(theString)), env)
		return
	}
	return BooleanWithValue(len(StringValue(theString)) == 0), nil
}

func SubstringImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	theString := Car(args)
	if !StringP(theString) {
		err = ProcessError(fmt.Sprintf("substring requires a string but was given %s.", String(theString)), env)
		return
	}
	stringValue := StringValue(theString)

	startObj := Cadr(args)
	if !IntegerP(startObj) {
		err = ProcessError(fmt.Sprintf("substring requires integer start but was given %s.", String(startObj)), env)
		return
	}
	startValue := int(IntegerValue(startObj))
	if startValue > len(stringValue) {
		err = ProcessError(fmt.Sprintf("substring requires start < length of the string."), env)
		return
	}

	endObj := Caddr(args)
	if !IntegerP(endObj) {
		err = ProcessError(fmt.Sprintf("substring requires integer end but was given %s.", String(endObj)), env)
		return
	}
	endValue := int(IntegerValue(endObj))
	if endValue > len(stringValue) {
		err = ProcessError(fmt.Sprintf("substring requires end < length of the string."), env)
		return
	}

	if startValue > endValue {
		err = ProcessError(fmt.Sprintf("substring requires start <= end."), env)
		return
	}

	return StringWithValue(stringValue[startValue:endValue]), nil
}

func SubstringpImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	substringObj := Car(args)
	if !StringP(substringObj) {
		err = ProcessError(fmt.Sprintf("substring? requires strings but was given %s.", String(substringObj)), env)
		return
	}
	substringValue := StringValue(substringObj)

	theString := Cadr(args)
	if !StringP(theString) {
		err = ProcessError(fmt.Sprintf("substring? requires strings but was given %s.", String(theString)), env)
		return
	}
	stringValue := StringValue(theString)

	return BooleanWithValue(strings.Contains(stringValue, substringValue)), nil
}

func StringPrefixpImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	prefixObj := Car(args)
	if !StringP(prefixObj) {
		err = ProcessError(fmt.Sprintf("string-prefix? requires a string but was given %s.", String(prefixObj)), env)
		return
	}
	prefixValue := StringValue(prefixObj)

	theString := Cadr(args)
	if !StringP(theString) {
		err = ProcessError(fmt.Sprintf("string-prefix? requires a string but was given %s.", String(theString)), env)
		return
	}
	stringValue := StringValue(theString)

	return BooleanWithValue(strings.HasPrefix(stringValue, prefixValue)), nil
}

func StringSuffixpImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	suffixObj := Car(args)
	if !StringP(suffixObj) {
		err = ProcessError(fmt.Sprintf("string-suffix? requires a string but was given %s.", String(suffixObj)), env)
		return
	}
	suffixValue := StringValue(suffixObj)

	theString := Cadr(args)
	if !StringP(theString) {
		err = ProcessError(fmt.Sprintf("string-suffix? requires a string but was given %s.", String(theString)), env)
		return
	}
	stringValue := StringValue(theString)

	return BooleanWithValue(strings.HasSuffix(stringValue, suffixValue)), nil
}

func stringProcessArgs(name string, caseInsensitive bool, args *Data, env *SymbolTableFrame) (string1 string, string2 string, err error) {
	string1Obj := Car(args)
	if !StringP(string1Obj) {
		err = ProcessError(fmt.Sprintf("%s requires a string but was given %s.", name, String(string1Obj)), env)
		return
	}
	if caseInsensitive {
		string1 = strings.ToLower(StringValue(string1Obj))
	} else {
		string1 = StringValue(string1Obj)
	}

	string2Obj := Cadr(args)
	if !StringP(string2Obj) {
		err = ProcessError(fmt.Sprintf("%s requires a string but was given %s.", name, String(string2Obj)), env)
		return
	}

	if caseInsensitive {
		string2 = strings.ToLower(StringValue(string2Obj))
	} else {
		string2 = StringValue(string2Obj)
	}

	return
}

func StringEqualImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	string1, string2, err := stringProcessArgs("string=?", false, args, env)
	if err == nil {
		result = BooleanWithValue(string1 == string2)
	}
	return
}

func StringEqualCiImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	string1, string2, err := stringProcessArgs("string-ci=?", true, args, env)
	if err == nil {
		result = BooleanWithValue(string1 == string2)
	}
	return
}

func StringLessThanImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	string1, string2, err := stringProcessArgs("string<?", false, args, env)
	if err == nil {
		result = BooleanWithValue(string1 < string2)
	}
	return
}

func StringLessThanCiImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	string1, string2, err := stringProcessArgs("string-ci<?", true, args, env)
	if err == nil {
		result = BooleanWithValue(string1 < string2)
	}
	return
}

func StringGreaterThanImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	string1, string2, err := stringProcessArgs("string>?", false, args, env)
	if err == nil {
		result = BooleanWithValue(string1 > string2)
	}
	return
}

func StringGreaterThanCiImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	string1, string2, err := stringProcessArgs("string-ci>?", true, args, env)
	if err == nil {
		result = BooleanWithValue(string1 > string2)
	}
	return
}

func StringLessThanEqualImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	string1, string2, err := stringProcessArgs("string<=?", false, args, env)
	if err == nil {
		result = BooleanWithValue(string1 <= string2)
	}
	return
}

func StringLessThanEqualCiImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	string1, string2, err := stringProcessArgs("string-ci<=?", true, args, env)
	if err == nil {
		result = BooleanWithValue(string1 <= string2)
	}
	return
}

func StringGreaterThanEqualImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	string1, string2, err := stringProcessArgs("string>=?", false, args, env)
	if err == nil {
		result = BooleanWithValue(string1 >= string2)
	}
	return
}

func StringGreaterThanEqualCiImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	string1, string2, err := stringProcessArgs("string-ci>=?", true, args, env)
	if err == nil {
		result = BooleanWithValue(string1 >= string2)
	}
	return
}

func ParseImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	str := First(args)
	if !StringP(str) {
		err = fmt.Errorf("parse requires its argument to be a string but it was %s", String(str))
		return
	}
	strValue := StringValue(str)

	return Parse(strValue)
}
