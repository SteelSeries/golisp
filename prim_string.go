// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the string primitive functions.

package golisp

import (
	"fmt"
	"strings"
	"unicode"
)

const (
	TrimLeft  = iota
	TrimBoth  = iota
	TrimRight = iota
)

func RegisterStringPrimitives() {
	MakeTypedPrimitiveFunction("string->list", "1", StringToListImpl, []uint32{StringType})
	MakeTypedPrimitiveFunction("list->string", "1", ListToStringImpl, []uint32{ConsCellType})

	MakeTypedPrimitiveFunction("string-ref", "2", StringRefImpl, []uint32{StringType, IntegerType})
	MakeTypedPrimitiveFunction("string-set!", "3", StringSetImpl, []uint32{StringType, IntegerType, CharacterType})

	MakeTypedPrimitiveFunction("string-split", "2", StringSplitImpl, []uint32{StringType, StringType})
	MakeTypedPrimitiveFunction("string-join", "1|2", StringJoinImpl, []uint32{ConsCellType, StringType})

	MakeTypedPrimitiveFunction("string-trim", "1|2", StringTrimImpl, []uint32{StringType, StringType})
	MakeTypedPrimitiveFunction("string-trim-left", "1|2", StringTrimLeftImpl, []uint32{StringType, StringType})
	MakeTypedPrimitiveFunction("string-trim-right", "1|2", StringTrimRightImpl, []uint32{StringType, StringType})

	MakeTypedPrimitiveFunction("string-capitalized?", "1", StringCapitalizedPImpl, []uint32{StringType})
	MakeTypedPrimitiveFunction("string-upper-case?", "1", StringUpperCasePImpl, []uint32{StringType})
	MakeTypedPrimitiveFunction("string-lower-case?", "1", StringLowerCasePImpl, []uint32{StringType})

	MakeTypedPrimitiveFunction("string-upcase", "1", StringUpcaseImpl, []uint32{StringType})
	MakeTypedPrimitiveFunction("string-upcase!", "1", StringUpcaseBangImpl, []uint32{StringType})
	MakeTypedPrimitiveFunction("substring-upcase!", "3", SubstringUpcaseBangImpl, []uint32{StringType, IntegerType, IntegerType})

	MakeTypedPrimitiveFunction("string-downcase", "1", StringDowncaseImpl, []uint32{StringType})
	MakeTypedPrimitiveFunction("string-downcase!", "1", StringDowncaseBangImpl, []uint32{StringType})
	MakeTypedPrimitiveFunction("substring-downcase!", "3", SubstringDowncaseBangImpl, []uint32{StringType, IntegerType, IntegerType})

	MakeTypedPrimitiveFunction("string-capitalize", "1", StringCapitalizeImpl, []uint32{StringType})
	MakeTypedPrimitiveFunction("string-capitalize!", "1", StringCapitalizeBangImpl, []uint32{StringType})
	MakeTypedPrimitiveFunction("substring-capitalize!", "3", SubstringCapitalizeBangImpl, []uint32{StringType, IntegerType, IntegerType})

	MakeTypedPrimitiveFunction("string-length", "1", StringLengthImpl, []uint32{StringType})

	MakeTypedPrimitiveFunction("substring", "3", SubstringImpl, []uint32{StringType, IntegerType, IntegerType})
	MakeTypedPrimitiveFunction("substring?", "2", SubstringpImpl, []uint32{StringType, StringType})

	MakeTypedPrimitiveFunction("string-prefix?", "2", StringPrefixpImpl, []uint32{StringType, StringType})
	MakeTypedPrimitiveFunction("string-suffix?", "2", StringSuffixpImpl, []uint32{StringType, StringType})

	MakeTypedPrimitiveFunction("string-compare", "5", StringCompareImpl, []uint32{StringType, StringType, FunctionType, FunctionType, FunctionType})
	MakeTypedPrimitiveFunction("string-compare-ci", "5", StringCompareCiImpl, []uint32{StringType, StringType, FunctionType, FunctionType, FunctionType})

	MakeTypedPrimitiveFunction("parse", "1", parseImpl, []uint32{StringType})
}

func StringToListImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	stringValue := StringValue(First(args))
	resultStrings := make([]*Data, len(stringValue))
	for i, s := range strings.Split(stringValue, "") {
		resultStrings[i] = CharacterWithValue(s)
	}

	result = ArrayToList(resultStrings)
	return
}

func ListToStringImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l := First(args)
	if !ListP(l) {
		err = ProcessError(fmt.Sprintf("list->string requires a list but was given %s.", String(l)), env)
		return
	}

	stringValues := make([]string, 0, Length(l))
	for cell := l; NotNilP(cell); cell = Cdr(cell) {
		c := Car(cell)
		if !CharacterP(c) {
			err = ProcessError(fmt.Sprintf("list->string requires a list of characters but found %s.", String(c)), env)
			return
		}
		stringValues = append(stringValues, StringValue(c))
	}
	result = StringWithValue(strings.Join(stringValues, ""))
	return
}

func StringRefImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	stringValue := StringValue(First(args))
	kValue := int(IntegerValue(Second(args)))

	if kValue < 0 || kValue >= len(stringValue) {
		err = ProcessError(fmt.Sprintf("string-ref was given an index that was out of range: %d.", kValue), env)
		return
	}

	result = CharacterWithValue(stringValue[kValue : kValue+1])
	return
}

func StringSetImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	theString := First(args)
	stringValue := StringValue(theString)
	kValue := int(IntegerValue(Second(args)))
	if kValue < 0 || kValue >= len(stringValue) {
		err = ProcessError(fmt.Sprintf("string-set! was given an index that was out of range: %d.", kValue), env)
		return
	}

	charValue := StringValue(Third(args))
	prefix := stringValue[:kValue]
	suffix := stringValue[kValue+1:]
	parts := []string{prefix, charValue, suffix}
	result = SetStringValue(theString, strings.Join(parts, ""))
	return
}

func checkAndExtractSubstringArgs(name string, args *Data, env *SymbolTableFrame) (stringValue string, startValue int, endValue int, err error) {
	stringValue = StringValue(First(args))
	startValue = int(IntegerValue(Second(args)))
	endValue = int(IntegerValue(Third(args)))

	if endValue > len(stringValue) {
		err = ProcessError(fmt.Sprintf("%s requires end < length of the string.", name), env)
		return
	}

	if startValue > endValue {
		err = ProcessError(fmt.Sprintf("%s requires start <= end.", name), env)
		return
	}

	return
}

func StringSplitImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	theString := Car(args)
	theSeparator := Cadr(args)
	pieces := strings.Split(StringValue(theString), StringValue(theSeparator))
	ary := make([]*Data, 0, len(pieces))
	for _, p := range pieces {
		ary = append(ary, StringWithValue(p))
	}
	return ArrayToList(ary), nil
}

func StringJoinImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	theStrings := First(args)
	if !ListP(theStrings) {
		err = ProcessError(fmt.Sprintf("string-join requires a list of strings to be joined but was given %s.", String(theStrings)), env)
		return
	}

	theSeparator := Second(args)
	separator := ""
	if !NilP(theSeparator) {
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
	theStringObj := First(args)
	theString := StringValue(theStringObj)
	var trimset string
	var f func(rune) bool

	trimWhiteSpaceFunc := func(c rune) bool {
		return unicode.IsSpace(c)
	}

	trimCustomCharsFunc := func(c rune) bool {
		return !strings.ContainsRune(trimset, c)
	}

	if Length(args) == 2 {
		trimset = StringValue(Second(args))
		f = trimCustomCharsFunc
	} else {
		f = trimWhiteSpaceFunc
	}

	switch lrb {
	case TrimLeft:
		return StringWithValue(strings.TrimLeftFunc(theString, f)), nil
	case TrimBoth:
		return StringWithValue(strings.TrimFunc(theString, f)), nil
	case TrimRight:
		return StringWithValue(strings.TrimRightFunc(theString, f)), nil
	default:
		return theStringObj, nil
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

func StringCapitalizedPImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	theString := StringValue(First(args))
	f := func(c rune) bool {
		return !unicode.IsLetter(c)
	}
	words := strings.FieldsFunc(theString, f)
	for index, word := range words {
		initial := word[:1]
		rest := word[1:]
		if (index == 0 && strings.ContainsAny(initial, "abcedfghijklmnopqrstuvwxyz")) || (index > 0 && strings.ContainsAny(rest, "ABCDEFGHIJKLMNOPQRSTUVWXYZ")) {
			result = LispFalse
			return
		}
	}
	result = LispTrue
	return
}

func StringLowerCasePImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	s := StringValue(First(args))
	if !strings.ContainsAny(s, "abcedfghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") {
		result = LispFalse
		return
	}

	result = BooleanWithValue(!strings.ContainsAny(s, "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
	return
}

func StringUpperCasePImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	s := StringValue(First(args))
	if !strings.ContainsAny(s, "abcedfghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") {
		result = LispFalse
		return
	}

	result = BooleanWithValue(!strings.ContainsAny(s, "abcedfghijklmnopqrstuvwxyz"))
	return
}

func StringUpcaseImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return StringWithValue(strings.ToUpper(StringValue(First(args)))), nil
}

func StringUpcaseBangImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	theString := First(args)
	return SetStringValue(theString, strings.ToUpper(StringValue(theString))), nil
}

func SubstringUpcaseBangImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	stringValue, startValue, endValue, err := checkAndExtractSubstringArgs("substring-upcase!", args, env)
	if err != nil {
		return
	}

	prefix := stringValue[:startValue]
	substring := strings.ToUpper(stringValue[startValue:endValue])
	suffix := stringValue[endValue:]
	parts := []string{prefix, substring, suffix}
	newString := strings.Join(parts, "")

	return SetStringValue(First(args), newString), nil
}

func StringDowncaseImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return StringWithValue(strings.ToLower(StringValue(First(args)))), nil
}

func StringDowncaseBangImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	theString := First(args)
	return SetStringValue(theString, strings.ToLower(StringValue(theString))), nil
}

func SubstringDowncaseBangImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	stringValue, startValue, endValue, err := checkAndExtractSubstringArgs("substring-downcase!", args, env)
	if err != nil {
		return
	}

	prefix := stringValue[:startValue]
	substring := strings.ToLower(stringValue[startValue:endValue])
	suffix := stringValue[endValue:]
	parts := []string{prefix, substring, suffix}
	newString := strings.Join(parts, "")

	return SetStringValue(First(args), newString), nil
}

func capitalize(s string) string {
	firstChar := s[:1]
	remainingChars := s[1:]
	parts := make([]string, 0, 2)
	parts = append(parts, strings.ToUpper(firstChar))
	parts = append(parts, strings.ToLower(remainingChars))
	return strings.Join(parts, "")
}

func StringCapitalizeImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return StringWithValue(capitalize(StringValue(First(args)))), nil
}

func StringCapitalizeBangImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	theString := First(args)
	return SetStringValue(theString, capitalize(StringValue(theString))), nil
}

func SubstringCapitalizeBangImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	stringValue, startValue, endValue, err := checkAndExtractSubstringArgs("substring-capitalize!", args, env)
	if err != nil {
		return
	}

	prefix := stringValue[:startValue]
	substring := strings.Title(strings.ToLower(stringValue[startValue:endValue]))
	suffix := stringValue[endValue:]
	parts := []string{prefix, substring, suffix}
	newString := strings.Join(parts, "")

	return SetStringValue(First(args), newString), nil
}

func StringLengthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	theString := Car(args)

	if !StringP(theString) {
		err = ProcessError(fmt.Sprintf("string-length requires a string but was given %s.", String(theString)), env)
		return
	}
	return IntegerWithValue(int64(len(StringValue(theString)))), nil
}

func SubstringImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	stringValue, startValue, endValue, err := checkAndExtractSubstringArgs("substring", args, env)
	if err != nil {
		return
	}

	return StringWithValue(stringValue[startValue:endValue]), nil
}

func SubstringpImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	substringValue := StringValue(First(args))
	stringValue := StringValue(Second(args))
	return BooleanWithValue(strings.Contains(stringValue, substringValue)), nil
}

func StringPrefixpImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	prefixValue := StringValue(First(args))
	stringValue := StringValue(Second(args))
	return BooleanWithValue(strings.HasPrefix(stringValue, prefixValue)), nil
}

func StringSuffixpImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	suffixValue := StringValue(First(args))
	stringValue := StringValue(Second(args))
	return BooleanWithValue(strings.HasSuffix(stringValue, suffixValue)), nil
}

func checkAndExtractStringArgs(name string, caseInsensitive bool, args *Data, env *SymbolTableFrame) (string1 string, string2 string, err error) {
	if caseInsensitive {
		string1 = strings.ToLower(StringValue(First(args)))
		string2 = strings.ToLower(StringValue(Second(args)))
	} else {
		string1 = StringValue(First(args))
		string2 = StringValue(Second(args))
	}
	return
}

func doStringComparison(name string, string1 string, string2 string, args *Data, env *SymbolTableFrame) (result *Data, err error) {
	ltProc := Third(args)
	eqProc := Fourth(args)
	gtProc := Fifth(args)

	switch strings.Compare(string1, string2) {
	case -1:
		return Apply(ltProc, EmptyCons(), env)
	case 0:
		return Apply(eqProc, EmptyCons(), env)
	case 1:
		return Apply(gtProc, EmptyCons(), env)
	}

	return
}

func StringCompareImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	string1, string2, err := checkAndExtractStringArgs("string-compare", false, args, env)
	if err == nil {
		return doStringComparison("string-compare", string1, string2, args, env)
	}

	return
}

func StringCompareCiImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	string1, string2, err := checkAndExtractStringArgs("string-compare-ci", true, args, env)

	if err == nil {
		return doStringComparison("string-compare-ci", string1, string2, args, env)
	}

	return
}

func parseImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return Parse(StringValue(First(args)))
}
