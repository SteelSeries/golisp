// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the bytearray primitive functions.

package golisp

import (
	"fmt"
	"unsafe"
)

func RegisterBytearrayPrimitives() {
	MakePrimitiveFunction("list-to-bytearray", 1, ListToBytesImpl)
	MakePrimitiveFunction("bytearray-to-list", 1, BytesToListImpl)
	MakePrimitiveFunction("replace-byte", 3, ReplaceByteImpl)
	MakePrimitiveFunction("replace-byte!", 3, ReplaceByteBangImpl)
	MakePrimitiveFunction("extract-byte", 2, ExtractByteImpl)
	MakePrimitiveFunction("append-bytes", -1, AppendBytesImpl)
	MakePrimitiveFunction("append-bytes!", -1, AppendBytesBangImpl)
	MakePrimitiveFunction("extract-bytes", 3, ExtractBytesImpl)
}

func ListToBytesImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if NilP(Car(args)) {
		err = ProcessError("Argument to ListToByutes can not be nil.", env)
		return
	}
	list, err := Eval(Car(args), env)
	if err != nil {
		return
	}

	bytes := make([]byte, 0, int(Length(list)))
	for c := list; NotNilP(c); c = Cdr(c) {
		var n *Data
		n, err = Eval(Car(c), env)
		if !IntegerP(n) {
			err = ProcessError(fmt.Sprintf("Byte arrays can only contain numbers, but found %v.", n), env)
			return
		}
		b := IntegerValue(n)
		if b > 255 {
			err = ProcessError(fmt.Sprintf("Byte arrays can only contain bytes, but found %d.", b), env)
			return
		}
		bytes = append(bytes, byte(b))
	}
	return ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&bytes)), nil
}

func BytesToListImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	dataByteObject, err := Eval(Car(args), env)
	if err != nil {
		panic(err)
	}
	if !ObjectP(dataByteObject) || TypeOfObject(dataByteObject) != "[]byte" {
		err = ProcessError(fmt.Sprintf("Bytearray object should return []byte but returned %s.", TypeOfObject(dataByteObject)), env)
		return
	}

	dataBytes := (*[]byte)(ObjectValue(dataByteObject))
	var bytes = make([]*Data, 0, len(*dataBytes))

	for _, b := range *dataBytes {
		bytes = append(bytes, IntegerWithValue(int64(b)))
	}

	result = ArrayToList(bytes)
	return
}

func internalReplaceByte(args *Data, env *SymbolTableFrame, makeCopy bool) (result *Data, err error) {

	if First(args) == nil {
		err = ProcessError("replace-byte requires a non-nil bytearray argument.", env)
		return
	}
	if Second(args) == nil {
		err = ProcessError("replace-byte requires a non-nil index argument.", env)
		return
	}
	if Third(args) == nil {
		err = ProcessError("replace-byte requires a non-nil value argument.", env)
		return
	}

	dataByteObject, err := Eval(First(args), env)
	if err != nil {
		panic(err)
	}
	if !ObjectP(dataByteObject) || TypeOfObject(dataByteObject) != "[]byte" {
		err = ProcessError(fmt.Sprintf("Bytearray object should return []byte but returned %s.", TypeOfObject(dataByteObject)), env)
		return
	}

	dataBytes := (*[]byte)(ObjectValue(dataByteObject))
	var newBytes *[]byte
	if makeCopy {
		temp := make([]byte, len(*dataBytes))
		newBytes = &temp
		copy(*newBytes, *dataBytes)
	} else {
		newBytes = dataBytes
	}

	indexObject, err := Eval(Second(args), env)
	if err != nil {
		panic(err)
	}
	if !IntegerP(indexObject) {
		panic(ProcessError("Bytearray index should be a number.", env))
	}
	index := int(IntegerValue(indexObject))

	if index >= len(*dataBytes) {
		err = ProcessError(fmt.Sprintf("replace-byte index was out of range. Was %d but bytearray has length of %d.", index, len(*dataBytes)), env)
		return
	}

	if WalkList(args, "add") == nil {
		err = ProcessError("replace-byte requires a non-nil value argument.", env)
		return
	}

	valueObject, err := Eval(Third(args), env)
	if err != nil {
		panic(err)
	}
	if !IntegerP(valueObject) {
		panic(ProcessError("Bytearray value should be a number.", env))
	}

	value := byte(IntegerValue(valueObject))

	if value > 255 {
		err = ProcessError(fmt.Sprintf("replace-byte value was not a byte. Was %d.", index), env)
		return
	}

	(*newBytes)[index] = value

	if makeCopy {
		result = ObjectWithTypeAndValue("[]byte", unsafe.Pointer(newBytes))
	} else {
		result = dataByteObject
	}
	return
}

func ReplaceByteImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return internalReplaceByte(args, env, true)
}

func ReplaceByteBangImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return internalReplaceByte(args, env, false)
}

func ExtractByteImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if Car(args) == nil {
		err = ProcessError("extract-byte requires a non-nil bytearray argument.", env)
		return
	}
	if Cadr(args) == nil {
		err = ProcessError("extract-byte requires a non-nil index argument.", env)
		return
	}

	dataByteObject, err := Eval(Car(args), env)
	if err != nil {
		panic(err)
	}
	if !ObjectP(dataByteObject) || TypeOfObject(dataByteObject) != "[]byte" {
		panic(ProcessError(fmt.Sprintf("Bytearray object should return []byte but returned %s.", TypeOfObject(dataByteObject)), env))
	}

	dataBytes := (*[]byte)(ObjectValue(dataByteObject))

	indexObject, err := Eval(Cadr(args), env)
	if err != nil {
		panic(err)
	}
	if !IntegerP(indexObject) {
		panic(ProcessError("Bytearray index should be a number.", env))
	}
	index := int(IntegerValue(indexObject))

	if index >= len(*dataBytes) {
		err = ProcessError(fmt.Sprintf("extract-byte index was out of range. Was %d but bytearray has length of %d.", index, len(*dataBytes)), env)
		return
	}

	extractedValue := (*dataBytes)[index]
	result = IntegerWithValue(int64(extractedValue))
	return
}

func internalAppendBytes(args *Data, env *SymbolTableFrame) (newBytes *[]byte, err error) {
	if Car(args) == nil {
		err = ProcessError("append-bytes requires a non-nil bytearray argument.", env)
		return
	}
	if Cadr(args) == nil {
		err = ProcessError("append-bytes requires a non-nil list of bytes to append.", env)
		return
	}

	dataByteObject, err := Eval(Car(args), env)
	if err != nil {
		panic(err)
	}
	if !ObjectP(dataByteObject) || TypeOfObject(dataByteObject) != "[]byte" {
		panic(ProcessError(fmt.Sprintf("Bytearray object should return []byte but returned %s.", TypeOfObject(dataByteObject)), env))
	}

	dataBytes := (*[]byte)(ObjectValue(dataByteObject))

	var extraByteObj *Data
	var evaledArg *Data
	if NilP(Cddr(args)) {
		evaledArg, err = Eval(Cadr(args), env)
		if err != nil {
			return
		}
		if ObjectP(evaledArg) && TypeOfObject(evaledArg) == "[]byte" {
			extraByteObj = evaledArg
		} else if ListP(evaledArg) {
			extraByteObj, err = ListToBytesImpl(InternalMakeList(QuoteIt(evaledArg)), env)
		} else {
			extraByteObj, err = ListToBytesImpl(InternalMakeList(QuoteIt(Cdr(args))), env)
		}
	} else {
		extraByteObj, err = ListToBytesImpl(InternalMakeList(QuoteIt(Cdr(args))), env)
	}

	if err != nil {
		return
	}

	extraBytes := (*[]byte)(ObjectValue(extraByteObj))

	temp := make([]byte, len(*dataBytes)+len(*extraBytes))
	newBytes = &temp
	copy(*newBytes, *dataBytes)
	copy((*newBytes)[len(*dataBytes):], *extraBytes)

	return
}

func AppendBytesImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	newBytesPtr, err := internalAppendBytes(args, env)
	if err != nil {
		return
	}
	result = ObjectWithTypeAndValue("[]byte", unsafe.Pointer(newBytesPtr))
	return
}

func AppendBytesBangImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	newBytesPtr, err := internalAppendBytes(args, env)
	dataByteObject, _ := Eval(Car(args), env)
	dataByteObject.Obj = unsafe.Pointer(newBytesPtr)
	result = dataByteObject
	return
}

func ExtractBytesImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if Car(args) == nil {
		err = ProcessError("extract-bytes requires a non-nil bytearray argument.", env)
		return
	}
	if Cadr(args) == nil {
		err = ProcessError("extract-bytes requires a non-nil index argument.", env)
		return
	}
	if Caddr(args) == nil {
		err = ProcessError("extract-bytes requires a non-nil number to extract argument.", env)
		return
	}

	dataByteObject, err := Eval(Car(args), env)
	if err != nil {
		panic(err)
	}
	if !ObjectP(dataByteObject) || TypeOfObject(dataByteObject) != "[]byte" {
		panic(ProcessError(fmt.Sprintf("Bytearray object should return []byte but returned %s.", TypeOfObject(dataByteObject)), env))
	}

	dataBytes := (*[]byte)(ObjectValue(dataByteObject))

	indexObject, err := Eval(Cadr(args), env)
	if err != nil {
		panic(err)
	}
	if !IntegerP(indexObject) {
		panic(ProcessError("Bytearray index should be a number.", env))
	}
	index := int(IntegerValue(indexObject))

	numToExtractObject, err := Eval(Caddr(args), env)
	if err != nil {
		panic(err)
	}
	if !IntegerP(numToExtractObject) {
		panic(ProcessError("Number to extract should be a number.", env))
	}
	numToExtract := int(IntegerValue(numToExtractObject))

	if index >= len(*dataBytes) {
		err = ProcessError(fmt.Sprintf("extract-bytes index was out of range. Was %d but bytearray has length of %d.", index, len(*dataBytes)), env)
		return
	}
	if index + numToExtract > len(*dataBytes) {
		err = ProcessError(fmt.Sprintf("extract-bytes final index was out of range.  Was %d but bytearray has length of %d.", index + numToExtract - 1, len(*dataBytes)), env)
		return
	}

	result, err = DropImpl(InternalMakeList(indexObject,dataByteObject),Global)
	if err != nil {
		return
	}
	result, err = TakeImpl(InternalMakeList(numToExtractObject,result), Global)
	return
}