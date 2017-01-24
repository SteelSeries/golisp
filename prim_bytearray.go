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
	MakePrimitiveFunction("bytearray?", "1", BytearrayPImpl)
	MakeTypedPrimitiveFunction("list->bytearray", "1", ListToBytesImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("bytearray->list", "1", BytesToListImpl, []uint32{BoxedObjectType})
	MakeTypedPrimitiveFunction("replace-byte", "3", ReplaceByteImpl, []uint32{BoxedObjectType, IntegerType, IntegerType})
	MakeTypedPrimitiveFunction("replace-byte!", "3", ReplaceByteBangImpl, []uint32{BoxedObjectType, IntegerType, IntegerType})
	MakeTypedPrimitiveFunction("extract-byte", "2", ExtractByteImpl, []uint32{BoxedObjectType, IntegerType})
	MakeTypedPrimitiveFunction("take-bytes", "2", TakeBytesImpl, []uint32{IntegerType, BoxedObjectType})
	MakeTypedPrimitiveFunction("drop-bytes", "2", DropBytesImpl, []uint32{IntegerType, BoxedObjectType})
	MakePrimitiveFunction("append-bytes", "*", AppendBytesImpl)
	MakePrimitiveFunction("append-bytes!", "*", AppendBytesBangImpl)
	MakeTypedPrimitiveFunction("extract-bytes", "3", ExtractBytesImpl, []uint32{BoxedObjectType, IntegerType, IntegerType})
}

func BytearrayPImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(BytearrayP(First(args))), nil
}

func ListToBytesImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	list := First(args)
	if NilP(list) {
		err = ProcessError("Argument to list->bytes can not be nil.", env)
		return
	}
	if !ListP(list) {
		err = ProcessError("Argument to list->bytes must be a proper list.", env)
		return
	}

	bytes := make([]byte, 0, int(Length(list)))
	for c := list; NotNilP(c); c = Cdr(c) {
		var n *Data
		n, err = Eval(Car(c), env)
		if !IntegerP(n) && !BytearrayP(n) {
			err = ProcessError(fmt.Sprintf("Byte arrays can only contain numbers, but found %v.", n), env)
			return
		}

		if IntegerP(n) {
			b := IntegerValue(n)
			if b < 0 || b > 255 {

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
	dataByteObject := First(args)
	if !BytearrayP(dataByteObject) {
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
	if !BytearrayP(dataByteObject) {
		err = ProcessError(fmt.Sprintf("replace-byte expects a bytearray as it's first argument but received %s.", ObjectType(dataByteObject)), env)
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

	index := int(IntegerValue(Second(args)))

	if index >= len(*dataBytes) {
		err = ProcessError(fmt.Sprintf("replace-byte index was out of range. Was %d but bytearray has length of %d.", index, len(*dataBytes)), env)
		return
	}

	if index < 0 {
		err = ProcessError(fmt.Sprintf("replace-byte index was out of range: %d.", index), env)
		return
	}

	value := IntegerValue(Third(args))
	if value < 0 || value > 255 {
		err = ProcessError(fmt.Sprintf("replace-byte value was not a byte. Was %d.", index), env)
		return
	}

	bytevalue := byte(value)

	(*newBytes)[index] = bytevalue

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
	dataByteObject := First(args)
	if !BytearrayP(dataByteObject) {
		err = ProcessError(fmt.Sprintf("Bytearray object should return []byte but returned %s.", ObjectType(dataByteObject)), env)
		return
	}

	dataBytes := (*[]byte)(ObjectValue(dataByteObject))

	index := int(IntegerValue(Second(args)))

	if index >= len(*dataBytes) {
		err = ProcessError(fmt.Sprintf("extract-byte index was out of range. Was %d but bytearray has length of %d.", index, len(*dataBytes)), env)
		return
	}

	if index < 0 {
		err = ProcessError(fmt.Sprintf("extract-byte index was out of range: %d.", index), env)
		return
	}

	extractedValue := (*dataBytes)[index]
	result = IntegerWithValue(int64(extractedValue))
	return
}

func TakeBytesImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	size := int(IntegerValue(Car(args)))
	ba := Cadr(args)
	if ObjectP(ba) && ObjectType(ba) == "[]byte" {
		dataBytes := (*[]byte)(ObjectValue(ba))
		var bytesToCopy []byte
		if size >= len(*dataBytes) {
			bytesToCopy = *dataBytes
		} else {
			bytesToCopy = (*dataBytes)[:size]
		}
		newBytes := make([]byte, len(bytesToCopy))
		for i, v := range bytesToCopy {
			newBytes[i] = v
		}
		result = ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&newBytes))
	} else {
		err = ProcessError("take-bytes requires a bytearray as its second argument.", env)
	}
	return
}

func DropBytesImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	size := int(IntegerValue(Car(args)))
	ba := Cadr(args)
	if ObjectP(ba) && ObjectType(ba) == "[]byte" {
		dataBytes := (*[]byte)(ObjectValue(ba))
		if size >= len(*dataBytes) {
			newBytes := make([]byte, 0)
			result = ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&newBytes))
		} else {
			newBytes := make([]byte, len(*dataBytes)-size)
			for i, v := range (*dataBytes)[size:] {
				newBytes[i] = v
			}
			result = ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&newBytes))
		}
	} else {
		err = ProcessError("drop-bytes requires a bytearray as its second argument.", env)
	}
	return
}

func internalAppendBytes(args *Data, env *SymbolTableFrame) (newBytes *[]byte, err error) {
	dataByteObject := First(args)
	if !BytearrayP(dataByteObject) {
		err = ProcessError(fmt.Sprintf("append-bytes extects first argument to be a bytearray, but was %s.", ObjectType(dataByteObject)), env)
		return
	}

	dataBytes := (*[]byte)(ObjectValue(dataByteObject))

	var extraByteObj *Data
	if Length(args) == 2 {
		secondArg := Cadr(args)
		if BytearrayP(secondArg) {
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
	if err != nil {
		return
	}
	dataByteObject := Car(args)
	objectData := BoxedObjectValue(dataByteObject)
	objectData.Obj = unsafe.Pointer(newBytesPtr)
	result = dataByteObject
	return
}

func ExtractBytesImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	dataByteObject := First(args)
	if !BytearrayP(dataByteObject) {
		err = ProcessError(fmt.Sprintf("Bytearray object should return []byte but returned %s.", ObjectType(dataByteObject)), env)
		return
	}

	dataBytes := (*[]byte)(ObjectValue(dataByteObject))

	index := int(IntegerValue(Second(args)))
	if index < 0 || index >= len(*dataBytes) {
		err = ProcessError(fmt.Sprintf("extract-bytes index was out of range. Was %d but bytearray has length of %d.", index, len(*dataBytes)), env)
		return
	}

	numToExtract := int(IntegerValue(Third(args)))
	if numToExtract < 0 {
		err = ProcessError("Number to extract can not be negative.", env)
		return
	}
	if index+numToExtract > len(*dataBytes) {
		err = ProcessError(fmt.Sprintf("extract-bytes final index was out of range.  Was %d but bytearray has length of %d.", index+numToExtract-1, len(*dataBytes)), env)
		return
	}

	result, err = DropBytesImpl(InternalMakeList(Second(args), dataByteObject), Global)
	if err != nil {
		return
	}
	result, err = TakeBytesImpl(InternalMakeList(Third(args), result), Global)
	return
}
