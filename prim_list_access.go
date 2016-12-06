// Copyright "2"014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the list access primitive functions.

package golisp

import (
	"errors"
	"fmt"
	"unsafe"
)

func RegisterListAccessPrimitives() {
	MakeTypedPrimitiveFunction("car", "1", CarImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("head", "1", CarImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("cdr", "1", CdrImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("rest", "1", CdrImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("tail", "1", CdrImpl, []uint32{ConsCellType})

	MakeTypedPrimitiveFunction("caar", "1", CaarImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("cadr", "1", CadrImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("cdar", "1", CdarImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("cddr", "1", CddrImpl, []uint32{ConsCellType})

	MakeTypedPrimitiveFunction("caaar", "1", CaaarImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("caadr", "1", CaadrImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("cadar", "1", CadarImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("caddr", "1", CaddrImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("cdaar", "1", CdaarImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("cdadr", "1", CdadrImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("cddar", "1", CddarImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("cdddr", "1", CdddrImpl, []uint32{ConsCellType})

	MakeTypedPrimitiveFunction("caaaar", "1", CaaaarImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("caaadr", "1", CaaadrImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("caadar", "1", CaadarImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("caaddr", "1", CaaddrImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("cadaar", "1", CadaarImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("cadadr", "1", CadadrImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("caddar", "1", CaddarImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("cadddr", "1", CadddrImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("cdaaar", "1", CdaaarImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("cdaadr", "1", CdaadrImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("cdadar", "1", CdadarImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("cdaddr", "1", CdaddrImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("cddaar", "1", CddaarImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("cddadr", "1", CddadrImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("cdddar", "1", CdddarImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("cddddr", "1", CddddrImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("general-car-cdr", "2", GeneralCarCdrImpl, []uint32{ConsCellType, IntegerType})

	MakeTypedPrimitiveFunction("first", "1", FirstImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("second", "1", SecondImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("third", "1", ThirdImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("fourth", "1", FourthImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("fifth", "1", FifthImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("sixth", "1", SixthImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("seventh", "1", SeventhImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("eighth", "1", EighthImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("ninth", "1", NinthImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("tenth", "1", TenthImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("last-pair", "1", LastPairImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("last", "1", LastImpl, []uint32{ConsCellType})

	MakeTypedPrimitiveFunction("nth", "2", NthImpl, []uint32{IntegerType, ConsCellType})
	MakeTypedPrimitiveFunction("take", "2", TakeImpl, []uint32{IntegerType, ConsCellType | BoxedObjectType})
	MakeTypedPrimitiveFunction("drop", "2", DropImpl, []uint32{IntegerType, ConsCellType | BoxedObjectType})
}

func CarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return WalkList(Car(args), "a"), nil
}

func CdrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return WalkList(Car(args), "d"), nil
}

func CaarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return WalkList(Car(args), "aa"), nil
}

func CadrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return WalkList(Car(args), "ad"), nil
}

func CdarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return WalkList(Car(args), "da"), nil
}

func CddrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return WalkList(Car(args), "dd"), nil
}

func CaaarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return WalkList(Car(args), "aaa"), nil
}

func CaadrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return WalkList(Car(args), "aad"), nil
}

func CadarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return WalkList(Car(args), "ada"), nil
}

func CaddrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return WalkList(Car(args), "add"), nil
}

func CdaarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return WalkList(Car(args), "daa"), nil
}

func CdadrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return WalkList(Car(args), "dad"), nil
}

func CddarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return WalkList(Car(args), "dda"), nil
}

func CdddrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return WalkList(Car(args), "ddd"), nil
}

func CaaaarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return WalkList(Car(args), "aaaa"), nil
}

func CaaadrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return WalkList(Car(args), "aaad"), nil
}

func CaadarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return WalkList(Car(args), "aada"), nil
}

func CaaddrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return WalkList(Car(args), "aadd"), nil
}

func CadaarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return WalkList(Car(args), "adaa"), nil
}

func CadadrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return WalkList(Car(args), "adad"), nil
}

func CaddarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return WalkList(Car(args), "adda"), nil
}

func CadddrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return WalkList(Car(args), "addd"), nil
}

func CdaaarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return WalkList(Car(args), "daaa"), nil
}

func CdaadrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return WalkList(Car(args), "daad"), nil
}

func CdadarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return WalkList(Car(args), "dada"), nil
}

func CdaddrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return WalkList(Car(args), "dadd"), nil
}

func CddaarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return WalkList(Car(args), "ddaa"), nil
}

func CddadrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return WalkList(Car(args), "ddad"), nil
}

func CdddarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return WalkList(Car(args), "ddda"), nil
}

func CddddrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return WalkList(Car(args), "dddd"), nil
}

func GeneralCarCdrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	list := Car(args)
	path := IntegerValue(Cadr(args))
	if path == 0 {
		err = errors.New("general-car-cdr requires a non-zero path specifier")
		return
	}
	for path != 1 {
		code := path & 0x1
		if code == 0 {
			list = Cdr(list)
		} else {
			list = Car(list)
		}
		path = path >> 1
	}
	return list, nil
}

func FirstImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return First(Car(args)), nil
}

func SecondImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return Second(Car(args)), nil
}

func ThirdImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return Third(Car(args)), nil
}

func FourthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return Fourth(Car(args)), nil
}

func FifthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return Fifth(Car(args)), nil
}

func SixthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return Sixth(Car(args)), nil
}

func SeventhImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return Seventh(Car(args)), nil
}

func EighthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return Eighth(Car(args)), nil
}

func NinthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return Ninth(Car(args)), nil
}

func TenthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return Tenth(Car(args)), nil
}

func LastPairImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if !(ListP(Car(args)) || DottedListP(Car(args))) {
		err = ProcessError(fmt.Sprintf("last-pair requires a non-circular list but received %s.", String(Car(args))), env)
		return
	}

	if Length(Car(args)) == 0 {
		err = ProcessError("last-pair requires a non-empty list but received nil.", env)
		return
	}

	return LastPair(Car(args)), nil
}

func LastImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if !ListP(Car(args)) || ListWithLoopP(Car(args)) {
		err = ProcessError(fmt.Sprintf("last requires a non-circular list but received %s.", String(Car(args))), env)
		return
	}
	return Car(LastPair(Car(args))), nil
}

func NthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	index := First(args)
	col := Second(args)
	if !ListP(col) {
		err = ProcessError(fmt.Sprintf("nth required a proper list but received %s.", String(col)), env)
		return
	}

	indexVal := int(IntegerValue(index))
	if indexVal < 0 || indexVal >= Length(col) {
		err = ProcessError(fmt.Sprintf("nth index out of bounds: %d.", indexVal), env)
		return
	}

	return Nth(col, indexVal), nil
}

func TakeImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	size := int(IntegerValue(Car(args)))
	l := Cadr(args)
	if ListP(l) {
		var items []*Data = make([]*Data, 0, Length(args))
		for i, cell := 0, l; i < size && NotNilP(cell); i, cell = i+1, Cdr(cell) {
			items = append(items, Car(cell))
		}
		result = ArrayToList(items)
	} else if ObjectP(l) && ObjectType(l) == "[]byte" {
		dataBytes := (*[]byte)(ObjectValue(l))
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
		err = ProcessError("take requires a proper list or bytearray as its second argument.", env)
	}
	return
}

func DropImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	size := int(IntegerValue(Car(args)))
	l := Cadr(args)

	if ListP(l) {
		var cell *Data
		var i int
		for i, cell = 0, l; i < size && NotNilP(cell); i, cell = i+1, Cdr(cell) {
		}
		result = cell
	} else if ObjectP(l) && ObjectType(l) == "[]byte" {
		dataBytes := (*[]byte)(ObjectValue(l))
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
		err = ProcessError("drop requires a proper list or bytearray as its second argument.", env)
	}
	return
}
