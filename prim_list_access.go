// Copyright "2"014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the list access primitive functions.

package golisp

import (
	"errors"
	"unsafe"
)

func RegisterListAccessPrimitives() {
	MakePrimitiveFunction("car", "1", CarImpl)
	MakePrimitiveFunction("head", "1", CarImpl)
	MakePrimitiveFunction("cdr", "1", CdrImpl)
	MakePrimitiveFunction("rest", "1", CdrImpl)
	MakePrimitiveFunction("tail", "1", CdrImpl)

	MakePrimitiveFunction("caar", "1", CaarImpl)
	MakePrimitiveFunction("cadr", "1", CadrImpl)
	MakePrimitiveFunction("cdar", "1", CdarImpl)
	MakePrimitiveFunction("cddr", "1", CddrImpl)

	MakePrimitiveFunction("caaar", "1", CaaarImpl)
	MakePrimitiveFunction("caadr", "1", CaadrImpl)
	MakePrimitiveFunction("cadar", "1", CadarImpl)
	MakePrimitiveFunction("caddr", "1", CaddrImpl)
	MakePrimitiveFunction("cdaar", "1", CdaarImpl)
	MakePrimitiveFunction("cdadr", "1", CdadrImpl)
	MakePrimitiveFunction("cddar", "1", CddarImpl)
	MakePrimitiveFunction("cdddr", "1", CdddrImpl)

	MakePrimitiveFunction("caaaar", "1", CaaaarImpl)
	MakePrimitiveFunction("caaadr", "1", CaaadrImpl)
	MakePrimitiveFunction("caadar", "1", CaadarImpl)
	MakePrimitiveFunction("caaddr", "1", CaaddrImpl)
	MakePrimitiveFunction("cadaar", "1", CadaarImpl)
	MakePrimitiveFunction("cadadr", "1", CadadrImpl)
	MakePrimitiveFunction("caddar", "1", CaddarImpl)
	MakePrimitiveFunction("cadddr", "1", CadddrImpl)
	MakePrimitiveFunction("cdaaar", "1", CdaaarImpl)
	MakePrimitiveFunction("cdaadr", "1", CdaadrImpl)
	MakePrimitiveFunction("cdadar", "1", CdadarImpl)
	MakePrimitiveFunction("cdaddr", "1", CdaddrImpl)
	MakePrimitiveFunction("cddaar", "1", CddaarImpl)
	MakePrimitiveFunction("cddadr", "1", CddadrImpl)
	MakePrimitiveFunction("cdddar", "1", CdddarImpl)
	MakePrimitiveFunction("cddddr", "1", CddddrImpl)
	MakePrimitiveFunction("general-car-cdr", "2", GeneralCarCdrImpl)

	MakePrimitiveFunction("first", "1", FirstImpl)
	MakePrimitiveFunction("second", "1", SecondImpl)
	MakePrimitiveFunction("third", "1", ThirdImpl)
	MakePrimitiveFunction("fourth", "1", FourthImpl)
	MakePrimitiveFunction("fifth", "1", FifthImpl)
	MakePrimitiveFunction("sixth", "1", SixthImpl)
	MakePrimitiveFunction("seventh", "1", SeventhImpl)
	MakePrimitiveFunction("eighth", "1", EighthImpl)
	MakePrimitiveFunction("ninth", "1", NinthImpl)
	MakePrimitiveFunction("tenth", "1", TenthImpl)

	MakePrimitiveFunction("nth", "2", NthImpl)
	MakePrimitiveFunction("take", "2", TakeImpl)
	MakePrimitiveFunction("drop", "2", DropImpl)

	MakePrimitiveFunction("list-ref", "2", ListRefImpl)
	MakePrimitiveFunction("list-head", "2", ListHeadImpl)
	MakePrimitiveFunction("list-tail", "2", ListTailImpl)
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

func NthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	col := Car(args)
	if !PairP(col) {
		err = ProcessError("First arg to nth must be a list", env)
		return
	}
	count := Cadr(args)
	if !IntegerP(count) {
		err = ProcessError("Second arg to nth must be a number", env)
		return
	}

	return Nth(col, int(IntegerValue(count))), nil
}

func TakeImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	n := Car(args)
	if !IntegerP(n) {
		err = ProcessError("take requires a number as its first argument.", env)
	}
	size := int(IntegerValue(n))

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
		err = ProcessError("take requires a list or bytearray as its second argument.", env)
	}
	return
}

func DropImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	n := Car(args)
	if !IntegerP(n) {
		err = ProcessError("drop requires a number as its first argument.", env)
	}
	size := int(IntegerValue(n))

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
		err = ProcessError("drop requires a list or bytearray as its second argument.", env)
	}
	return
}

func ListRefImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	col := Car(args)
	if !PairP(col) {
		err = ProcessError("First arg to list-ref must be a list", env)
		return
	}
	count := Cadr(args)
	if !IntegerP(count) {
		err = ProcessError("Second arg to list-ref must be a number", env)
		return
	}

	return Nth(col, int(IntegerValue(count))+1), nil
}

func ListHeadImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	n := Cadr(args)
	if !IntegerP(n) {
		err = ProcessError("list-head requires a number as its second argument.", env)
	}
	size := int(IntegerValue(n))

	l := Car(args)
	if ListP(l) {
		var items []*Data = make([]*Data, 0, Length(args))
		for i, cell := 0, l; i < size && NotNilP(cell); i, cell = i+1, Cdr(cell) {
			items = append(items, Car(cell))
		}
		result = ArrayToList(items)
	} else {
		err = ProcessError("list-head requires a list as its first argument.", env)
	}
	return
}

func ListTailImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	n := Cadr(args)
	if !IntegerP(n) {
		err = ProcessError("list-tail requires a number as its second argument.", env)
	}
	size := int(IntegerValue(n))

	l := Car(args)

	if ListP(l) {
		var cell *Data
		var i int
		for i, cell = 0, l; i < size && NotNilP(cell); i, cell = i+1, Cdr(cell) {
		}
		result = cell
	} else {
		err = ProcessError("list-tail requires a list or bytearray as its first argument.", env)
	}
	return
}
