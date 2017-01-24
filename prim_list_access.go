// Copyright "2"014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the list access primitive functions.

package golisp

import (
	"errors"
	"fmt"
)

func RegisterListAccessPrimitives() {
	MakeTypedPrimitiveFunction("car", "1", CarImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("cdr", "1", CdrImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("general-car-cdr", "2", GeneralCarCdrImpl, []uint32{ConsCellType, IntegerType})
	MakeTypedPrimitiveFunction("last-pair", "1", LastPairImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("last", "1", LastImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("nth", "2", NthImpl, []uint32{IntegerType, ConsCellType})
}

func CarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return Car(Car(args)), nil
}

func CdrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return Cdr(Car(args)), nil
}

func GeneralCarCdrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	list := Car(args)
	path := IntegerValue(Cadr(args))
	if path == 0 {
		err = errors.New("general-car-cdr requires a non-zero path specifier")
		return
	}
	for path != 1 {
		if (path & 0x1) == 0 {
			list = Cdr(list)
		} else {
			list = Car(list)
		}
		path = path >> 1
	}
	return list, nil
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
