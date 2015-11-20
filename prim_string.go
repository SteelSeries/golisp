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
	MakePrimitiveFunction("string->list", "1", StringToListImpl)
	MakePrimitiveFunction("list->string", "1", ListToStringImpl)

	MakePrimitiveFunction("string-ref", "2", StringRefImpl)
	MakePrimitiveFunction("string-set!", "3", StringSetImpl)

	MakePrimitiveFunction("string-split", "2", StringSplitImpl)
	MakePrimitiveFunction("string-join", "1|2", StringJoinImpl)

	MakePrimitiveFunction("string-trim", "1|2", StringTrimImpl)
	MakePrimitiveFunction("string-trim-left", "1|2", StringTrimLeftImpl)
	MakePrimitiveFunction("string-trim-right", "1|2", StringTrimRightImpl)

	MakePrimitiveFunction("string-capitalized?", "1", StringCapitalizedPImpl)
	MakePrimitiveFunction("string-upper-case?", "1", StringUpperCasePImpl)
	MakePrimitiveFunction("string-lower-case?", "1", StringLowerCasePImpl)

	MakePrimitiveFunction("string-upcase", "1", StringUpcaseImpl)
	MakePrimitiveFunction("string-upcase!", "1", StringUpcaseBangImpl)
	MakePrimitiveFunction("substring-upcase!", "3", SubstringUpcaseBangImpl)

	MakePrimitiveFunction("string-downcase", "1", StringDowncaseImpl)
	MakePrimitiveFunction("string-downcase!", "1", StringDowncaseBangImpl)
	MakePrimitiveFunction("substring-downcase!", "3", SubstringDowncaseBangImpl)

	MakePrimitiveFunction("string-capitalize", "1", StringCapitalizeImpl)
	MakePrimitiveFunction("string-capitalize!", "1", StringCapitalizeBangImpl)
	MakePrimitiveFunction("substring-capitalize!", "3", SubstringCapitalizeBangImpl)

	MakePrimitiveFunction("string-length", "1", StringLengthImpl)

	MakePrimitiveFunction("substring", "3", SubstringImpl)
	MakePrimitiveFunction("substring?", "2", SubstringpImpl)

	MakePrimitiveFunction("string-prefix?", "2", StringPrefixpImpl)
	MakePrimitiveFunction("string-suffix?", "2", StringSuffixpImpl)

	MakePrimitiveFunction("string-compare", "5", StringCompareImpl)
	MakePrimitiveFunction("string-compare-ci", "5", StringCompareCiImpl)
}

func StringToListImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	theString := First(args)

	if !StringP(theString) {
		err = ProcessError(fmt.Sprintf("string->list requires a string but was given %s.", String(theString)), env)
		return
	}

	stringValue := StringValue(theString)
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
	theString := First(args)
	if !StringP(theString) {
		err = ProcessError(fmt.Sprintf("string-ref requires a string but was given %s.", String(theString)), env)
		return
	}
	stringValue := StringValue(theString)

	kObj := Second(args)
	if !IntegerP(kObj) {
		err = ProcessError(fmt.Sprintf("string-ref requires integer index but was given %s.", String(kObj)), env)
		return
	}
	kValue := int(IntegerValue(kObj))

	if kValue < 0 || kValue >= len(stringValue) {
		err = ProcessError(fmt.Sprintf("string-ref was given an index that was out of range: %d.", kValue), env)
		return
	}

	result = CharacterWithValue(stringValue[kValue : kValue+1])
	return
}

func StringSetImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	theString := First(args)
	if !StringP(theString) {
		err = ProcessError(fmt.Sprintf("string-set! requires a string but was given %s.", String(theString)), env)
		return
	}
	stringValue := StringValue(theString)

	kObj := Second(args)
	if !IntegerP(kObj) {
		err = ProcessError(fmt.Sprintf("string-set! requires integer index but was given %s.", String(kObj)), env)
		return
	}
	kValue := int(IntegerValue(kObj))

	if kValue < 0 || kValue >= len(stringValue) {
		err = ProcessError(fmt.Sprintf("string-set! was given an index that was out of range: %d.", kValue), env)
		return
	}

	charObj := Third(args)
	if !CharacterP(charObj) {
		err = ProcessError(fmt.Sprintf("string-set! requires a Character replacement value but was given %s.", String(charObj)), env)
		return
	}
	charValue := StringValue(charObj)

	prefix := stringValue[:kValue]
	suffix := stringValue[kValue+1:]
	parts := []string{prefix, charValue, suffix}
	result = SetStringValue(theString, strings.Join(parts, ""))
	return
}

func checkAndExtractSubstringArgs(name string, args *Data, env *SymbolTableFrame) (stringValue string, startValue int, endValue int, err error) {
	theString := First(args)

	if !StringP(theString) {
		err = ProcessError(fmt.Sprintf("%s requires a string but was given %s.", name, String(theString)), env)
		return
	}
	stringValue = StringValue(theString)

	startObj := Second(args)
	if !IntegerP(startObj) {
		err = ProcessError(fmt.Sprintf("%s requires integer start but was given %s.", name, String(startObj)), env)
		return
	}
	startValue = int(IntegerValue(startObj))
	if startValue > len(stringValue) {
		err = ProcessError(fmt.Sprintf("%s requires start < length of the string.", name), env)
		return
	}

	endObj := Third(args)
	if !IntegerP(endObj) {
		err = ProcessError(fmt.Sprintf("%s requires integer end but was given %s.", name, String(endObj)), env)
		return
	}
	endValue = int(IntegerValue(endObj))
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
	var f func(rune) bool

	trimWhiteSpaceFunc := func(c rune) bool {
		return unicode.IsSpace(c)
	}

	trimCustomCharsFunc := func(c rune) bool {
		return !strings.ContainsRune(trimset, c)
	}

	if Length(args) == 2 {
		theTrimSet := Cadr(args)
		if !StringP(theTrimSet) {
			err = ProcessError(fmt.Sprintf("string-trim requires a string set of trim characters but was given %s.", String(theTrimSet)), env)
			return
		}

		trimset = StringValue(theTrimSet)
		f = trimCustomCharsFunc
	} else {
		f = trimWhiteSpaceFunc
	}

	switch lrb {
	case TrimLeft:
		return StringWithValue(strings.TrimLeftFunc(StringValue(theString), f)), nil
	case TrimBoth:
		return StringWithValue(strings.TrimFunc(StringValue(theString), f)), nil
	case TrimRight:
		return StringWithValue(strings.TrimRightFunc(StringValue(theString), f)), nil
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

func StringCapitalizedPImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	theString := Car(args)
	if !StringP(theString) {
		err = ProcessError(fmt.Sprintf("string-capitalized? requires a string but was given %s.", String(theString)), env)
		return
	}
	f := func(c rune) bool {
		return !unicode.IsLetter(c)
	}
	words := strings.FieldsFunc(StringValue(theString), f)
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
	theString := Car(args)
	if !StringP(theString) {
		err = ProcessError(fmt.Sprintf("string-lower-case? requires a string but was given %s.", String(theString)), env)
		return
	}

	s := StringValue(theString)
	if !strings.ContainsAny(s, "abcedfghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") {
		result = LispFalse
		return
	}

	result = BooleanWithValue(!strings.ContainsAny(s, "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
	return
}

func StringUpperCasePImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	theString := Car(args)
	if !StringP(theString) {
		err = ProcessError(fmt.Sprintf("string-upper-case? requires a string but was given %s.", String(theString)), env)
		return
	}

	s := StringValue(theString)
	if !strings.ContainsAny(s, "abcedfghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") {
		result = LispFalse
		return
	}

	result = BooleanWithValue(!strings.ContainsAny(s, "abcedfghijklmnopqrstuvwxyz"))
	return
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
	stringValue, startValue, endValue, err := checkAndExtractSubstringArgs("substring-capitalize!", args, env)
	if err != nil {
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

func checkAndExtractStringArgs(name string, caseInsensitive bool, args *Data, env *SymbolTableFrame) (string1 string, string2 string, err error) {
	string1Obj := First(args)
	if !StringP(string1Obj) {
		err = ProcessError(fmt.Sprintf("%s requires a string but was given %s.", name, String(string1Obj)), env)
		return
	}
	if caseInsensitive {
		string1 = strings.ToLower(StringValue(string1Obj))
	} else {
		string1 = StringValue(string1Obj)
	}

	string2Obj := Second(args)
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

func doStringComparison(name string, string1 string, string2 string, args *Data, env *SymbolTableFrame) (result *Data, err error) {
	ltProc := Third(args)
	if !FunctionP(ltProc) {
		err = ProcessError(fmt.Sprintf("string-compare-ci requires a function for argument 3 but was given %s.", String(ltProc)), env)
		return
	}

	eqProc := Fourth(args)
	if !FunctionP(eqProc) {
		err = ProcessError(fmt.Sprintf("string-compare-ci requires a function for argument 4 but was given %s.", String(eqProc)), env)
		return
	}

	gtProc := Fifth(args)
	if !FunctionP(gtProc) {
		err = ProcessError(fmt.Sprintf("string-compare-ci requires a function for argument 4 but was given %s.", String(gtProc)), env)
		return
	}

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
		result, err = doStringComparison("string-compare", string1, string2, args, env)
	}

	return
}

func StringCompareCiImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	string1, string2, err := checkAndExtractStringArgs("string-compare-ci", true, args, env)

	if err == nil {
		result, err = doStringComparison("string-compare-ci", string1, string2, args, env)
	}

	return
}
