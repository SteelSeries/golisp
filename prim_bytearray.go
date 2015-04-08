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
	MakePrimitiveFunction("list-to-bytearray", "1", ListToBytesImpl)
	MakePrimitiveFunction("bytearray-to-list", "1", BytesToListImpl)
	MakePrimitiveFunction("replace-byte", "3", ReplaceByteImpl)
	MakePrimitiveFunction("replace-byte!", "3", ReplaceByteBangImpl)
	MakePrimitiveFunction("extract-byte", "2", ExtractByteImpl)
	MakePrimitiveFunction("append-bytes", "*", AppendBytesImpl)
	MakePrimitiveFunction("append-bytes!", "*", AppendBytesBangImpl)
	MakePrimitiveFunction("extract-bytes", "3", ExtractBytesImpl)
}

func ListToBytesImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	list := Car(args)
	if NilP(list) {
		err = ProcessError("Argument to ListToBytes can not be nil.", env)
		return
	}
	if !ListP(list) {
		err = ProcessError("Argument to ListToBytes must be a list.", env)
		return
	}

	bytes := make([]byte, 0, int(Length(list)))
	for c := list; NotNilP(c); c = Cdr(c) {
		var n *Data
		n, err = Eval(Car(c), env)
		if !IntegerP(n) && !(ObjectP(n) && ObjectType(n) == "[]byte") {
			err = ProcessError(fmt.Sprintf("Byte arrays can only contain numbers, but found %v.", n), env)
			return
		}

		if IntegerP(n) {
			b := IntegerValue(n)
			if b > 255 {

				err = ProcessError(fmt.Sprintf("Byte arrays can only contain bytes, but found %d.", b), env)
				return
			}
			bytes = append(bytes, byte(b))
		} else {
			otherArrayBytes := *(*[]byte)(ObjectValue(n))
			bytes = append(bytes, otherArrayBytes...)
		}
	}
	return ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&bytes)), nil
}

func BytesToListImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	dataByteObject := Car(args)
	if !ObjectP(dataByteObject) || ObjectType(dataByteObject) != "[]byte" {
		err = ProcessError(fmt.Sprintf("Bytearray object should return []byte but returned %s.", ObjectType(dataByteObject)), env)
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
	dataByteObject := First(args)
	if !ObjectP(dataByteObject) || ObjectType(dataByteObject) != "[]byte" {
		err = ProcessError(fmt.Sprintf("Bytearray object should return []byte but returned %s.", ObjectType(dataByteObject)), env)
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

	indexObject := Second(args)
	if !IntegerP(indexObject) {
		err = ProcessError("Bytearray index should be a number.", env)
		return
	}
	index := int(IntegerValue(indexObject))

	if index >= len(*dataBytes) {
		err = ProcessError(fmt.Sprintf("replace-byte index was out of range. Was %d but bytearray has length of %d.", index, len(*dataBytes)), env)
		return
	}

	valueObject := Third(args)
	if !IntegerP(valueObject) {
		err = ProcessError("Bytearray value should be a number.", env)
		return
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
	dataByteObject := Car(args)
	if !ObjectP(dataByteObject) || ObjectType(dataByteObject) != "[]byte" {
		err = ProcessError(fmt.Sprintf("Bytearray object should return []byte but returned %s.", ObjectType(dataByteObject)), env)
		return
	}

	dataBytes := (*[]byte)(ObjectValue(dataByteObject))

	indexObject := Cadr(args)
	if !IntegerP(indexObject) {
		err = ProcessError("Bytearray index should be a number.", env)
		return
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
	dataByteObject := Car(args)
	if !ObjectP(dataByteObject) || ObjectType(dataByteObject) != "[]byte" {
		err = ProcessError(fmt.Sprintf("Bytearray object should return []byte but returned %s.", ObjectType(dataByteObject)), env)
		return
	}

	dataBytes := (*[]byte)(ObjectValue(dataByteObject))

	var extraByteObj *Data
	if Length(args) == 2 {
		secondArg := Cadr(args)
		if err != nil {
			return
		}
		if ObjectP(secondArg) && ObjectType(secondArg) == "[]byte" {
			extraByteObj = secondArg
		} else if ListP(secondArg) {
			extraByteObj, err = ListToBytesImpl(InternalMakeList(secondArg), env)
		} else {
			extraByteObj, err = ListToBytesImpl(InternalMakeList(Cdr(args)), env)
		}
	} else {
		extraByteObj, err = ListToBytesImpl(InternalMakeList(Cdr(args)), env)
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
	dataByteObject := Car(args)
	objectData := BoxedObjectValue(dataByteObject)
	objectData.Obj = unsafe.Pointer(newBytesPtr)
	result = dataByteObject
	return
}

func ExtractBytesImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	dataByteObject := Car(args)
	if !ObjectP(dataByteObject) || ObjectType(dataByteObject) != "[]byte" {
		err = ProcessError(fmt.Sprintf("Bytearray object should return []byte but returned %s.", ObjectType(dataByteObject)), env)
		return
	}

	dataBytes := (*[]byte)(ObjectValue(dataByteObject))

	indexObject := Cadr(args)
	if !IntegerP(indexObject) {
		err = ProcessError("Bytearray index should be a number.", env)
		return
	}
	index := int(IntegerValue(indexObject))

	numToExtractObject := Caddr(args)
	if !IntegerP(numToExtractObject) {
		err = ProcessError("Number to extract should be a number.", env)
		return
	}
	numToExtract := int(IntegerValue(numToExtractObject))

	if index >= len(*dataBytes) {
		err = ProcessError(fmt.Sprintf("extract-bytes index was out of range. Was %d but bytearray has length of %d.", index, len(*dataBytes)), env)
		return
	}
	if index+numToExtract > len(*dataBytes) {
		err = ProcessError(fmt.Sprintf("extract-bytes final index was out of range.  Was %d but bytearray has length of %d.", index+numToExtract-1, len(*dataBytes)), env)
		return
	}

	result, err = DropImpl(InternalMakeList(indexObject, dataByteObject), Global)
	if err != nil {
		return
	}
	result, err = TakeImpl(InternalMakeList(numToExtractObject, result), Global)
	return
}
