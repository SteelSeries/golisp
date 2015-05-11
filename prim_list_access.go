// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the list access primitive functions.

package golisp

import (
	"unsafe"
)

func RegisterListAccessPrimitives() {
	MakePrimitiveFunction("car", 1, CarImpl)
	MakePrimitiveFunction("head", 1, CarImpl)
	MakePrimitiveFunction("cdr", 1, CdrImpl)
	MakePrimitiveFunction("rest", 1, CdrImpl)
	MakePrimitiveFunction("tail", 1, CdrImpl)

	MakePrimitiveFunction("caar", 1, CaarImpl)
	MakePrimitiveFunction("cadr", 1, CadrImpl)
	MakePrimitiveFunction("cdar", 1, CdarImpl)
	MakePrimitiveFunction("cddr", 1, CddrImpl)

	MakePrimitiveFunction("caaar", 1, CaaarImpl)
	MakePrimitiveFunction("caadr", 1, CaadrImpl)
	MakePrimitiveFunction("cadar", 1, CadarImpl)
	MakePrimitiveFunction("caddr", 1, CaddrImpl)
	MakePrimitiveFunction("cdaar", 1, CdaarImpl)
	MakePrimitiveFunction("cdadr", 1, CdadrImpl)
	MakePrimitiveFunction("cddar", 1, CddarImpl)
	MakePrimitiveFunction("cdddr", 1, CdddrImpl)

	MakePrimitiveFunction("caaaar", 1, CaaaarImpl)
	MakePrimitiveFunction("caaadr", 1, CaaadrImpl)
	MakePrimitiveFunction("caadar", 1, CaadarImpl)
	MakePrimitiveFunction("caaddr", 1, CaaddrImpl)
	MakePrimitiveFunction("cadaar", 1, CadaarImpl)
	MakePrimitiveFunction("cadadr", 1, CadadrImpl)
	MakePrimitiveFunction("caddar", 1, CaddarImpl)
	MakePrimitiveFunction("cadddr", 1, CadddrImpl)
	MakePrimitiveFunction("cdaaar", 1, CdaaarImpl)
	MakePrimitiveFunction("cdaadr", 1, CdaadrImpl)
	MakePrimitiveFunction("cdadar", 1, CdadarImpl)
	MakePrimitiveFunction("cdaddr", 1, CdaddrImpl)
	MakePrimitiveFunction("cddaar", 1, CddaarImpl)
	MakePrimitiveFunction("cddadr", 1, CddadrImpl)
	MakePrimitiveFunction("cdddar", 1, CdddarImpl)
	MakePrimitiveFunction("cddddr", 1, CddddrImpl)

	MakePrimitiveFunction("first", 1, FirstImpl)
	MakePrimitiveFunction("second", 1, SecondImpl)
	MakePrimitiveFunction("third", 1, ThirdImpl)
	MakePrimitiveFunction("fourth", 1, FourthImpl)
	MakePrimitiveFunction("fifth", 1, FifthImpl)
	MakePrimitiveFunction("sixth", 1, SixthImpl)
	MakePrimitiveFunction("seventh", 1, SeventhImpl)
	MakePrimitiveFunction("eighth", 1, EighthImpl)
	MakePrimitiveFunction("ninth", 1, NinthImpl)
	MakePrimitiveFunction("tenth", 1, TenthImpl)

	MakePrimitiveFunction("nth", 2, NthImpl)
	MakePrimitiveFunction("take", 2, TakeImpl)
	MakePrimitiveFunction("drop", 2, DropImpl)

	MakePrimitiveFunction("last-pair", 1, LastPairImpl)
}

func CarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return WalkList(l, "a"), nil
}

func CdrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return WalkList(l, "d"), nil
}

func CaarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return WalkList(l, "aa"), nil
}

func CadrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return WalkList(l, "ad"), nil
}

func CdarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return WalkList(l, "da"), nil
}

func CddrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return WalkList(l, "dd"), nil
}

func CaaarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return WalkList(l, "aaa"), nil
}

func CaadrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return WalkList(l, "aad"), nil
}

func CadarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return WalkList(l, "ada"), nil
}

func CaddrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return WalkList(l, "add"), nil
}

func CdaarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return WalkList(l, "daa"), nil
}

func CdadrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return WalkList(l, "dad"), nil
}

func CddarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return WalkList(l, "dda"), nil
}

func CdddrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return WalkList(l, "ddd"), nil
}

func CaaaarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return WalkList(l, "aaaa"), nil
}

func CaaadrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return WalkList(l, "aaad"), nil
}

func CaadarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return WalkList(l, "aada"), nil
}

func CaaddrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return WalkList(l, "aadd"), nil
}

func CadaarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return WalkList(l, "adaa"), nil
}

func CadadrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return WalkList(l, "adad"), nil
}

func CaddarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return WalkList(l, "adda"), nil
}

func CadddrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return WalkList(l, "addd"), nil
}

func CdaaarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return WalkList(l, "daaa"), nil
}

func CdaadrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return WalkList(l, "daad"), nil
}

func CdadarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return WalkList(l, "dada"), nil
}

func CdaddrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return WalkList(l, "dadd"), nil
}

func CddaarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return WalkList(l, "ddaa"), nil
}

func CddadrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return WalkList(l, "ddad"), nil
}

func CdddarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return WalkList(l, "ddda"), nil
}

func CddddrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return WalkList(l, "dddd"), nil
}

func FirstImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return First(l), nil
}

func SecondImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return Second(l), nil
}

func ThirdImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return Third(l), nil
}

func FourthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return Fourth(l), nil
}

func FifthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return Fifth(l), nil
}

func SixthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return Sixth(l), nil
}

func SeventhImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return Seventh(l), nil
}

func EighthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return Eighth(l), nil
}

func NinthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return Ninth(l), nil
}

func TenthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	return Tenth(l), nil
}

func NthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	col, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !PairP(col) {
		err = ProcessError("First arg to nth must be a list", env)
		return
	}
	count, err := Eval(Cadr(args), env)
	if err != nil {
		return
	}
	if !IntegerP(count) {
		err = ProcessError("Second arg to nth must be a number", env)
		return
	}

	return Nth(col, int(IntegerValue(count))), nil
}

func TakeImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	n, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !IntegerP(n) {
		err = ProcessError("take requires a number as its first argument.", env)
	}
	size := int(IntegerValue(n))

	l, err := Eval(Cadr(args), env)
	if err != nil {
		return
	}
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
	n, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !IntegerP(n) {
		err = ProcessError("drop requires a number as its first argument.", env)
	}
	size := int(IntegerValue(n))

	l, err := Eval(Cadr(args), env)
	if err != nil {
		return
	}

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

func LastPairImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}

	if NilP(l) {
		err = ProcessError("last-pair requires a non-empty list as its argument.", env)
		return
	}

	if !ListP(l) {
		err = ProcessError("last-pair requires a list as its argument.", env)
		return
	}

	var cell *Data
	for cell = l; NotNilP(Cdr(cell)) && PairP(Cdr(cell)); cell = Cdr(cell) {
	}

	return cell, nil
}
