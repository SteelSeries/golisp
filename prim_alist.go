// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the association list primitive functions.

package golisp

func RegisterAListPrimitives() {
	MakeTypedPrimitiveFunction("acons", "2|3", AconsImpl, []uint32{AnyType, AnyType, ConsCellType})
	MakeTypedPrimitiveFunction("pairlis", "2|3", PairlisImpl, []uint32{ConsCellType, ConsCellType, ConsCellType})
	MakeTypedPrimitiveFunction("assq", "2", AssqImpl, []uint32{AnyType, ConsCellType})
	MakeTypedPrimitiveFunction("assv", "2", AssvImpl, []uint32{AnyType, ConsCellType})
	MakeTypedPrimitiveFunction("assoc", "2", AssocImpl, []uint32{AnyType, ConsCellType})
	MakeTypedPrimitiveFunction("dissq", "2", DissqImpl, []uint32{AnyType, ConsCellType})
	MakeTypedPrimitiveFunction("dissv", "2", DissvImpl, []uint32{AnyType, ConsCellType})
	MakeTypedPrimitiveFunction("dissoc", "2", DissocImpl, []uint32{AnyType, ConsCellType})
	MakeTypedPrimitiveFunction("rassoc", "2", RassocImpl, []uint32{AnyType, ConsCellType})
}

func AconsImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	key := First(args)
	value := Second(args)

	var alist *Data = nil
	if Length(args) == 3 {
		alist = Third(args)
	}

	return Acons(key, value, alist), nil
}

func PairlisImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	keys := First(args)
	values := Second(args)

	if Length(keys) != Length(values) {
		err = ProcessError("Pairlis requires the same number of keys and values", env)
		return
	}

	result = Third(args)

	for keyCell, valueCell := keys, values; NotNilP(keyCell); keyCell, valueCell = Cdr(keyCell), Cdr(valueCell) {
		key := Car(keyCell)
		if NilP(key) {
			err = ProcessError("pairlis found a nil assoc list keys", env)
		}
		value := Car(valueCell)
		result = Acons(key, value, result)
	}

	return
}

func AssqImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	key := First(args)
	alist := Second(args)
	return Assq(key, alist)
}

func AssvImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	key := First(args)
	alist := Second(args)
	return Assv(key, alist)
}

func AssocImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	key := First(args)
	alist := Second(args)
	return Assoc(key, alist)
}

func RassocImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	value := First(args)
	list := Second(args)

	for c := list; NotNilP(c); c = Cdr(c) {
		pair := Car(c)
		if !PairP(pair) && !DottedPairP(pair) {
			err = ProcessError("Assoc list must consist of dotted pairs", env)
			return
		}
		if IsEqual(Cdr(pair), value) {
			result = pair
			return
		}
	}
	return
}

func DissqImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	key := First(args)
	list := Second(args)
	return Dissq(key, list)
}

func DissvImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	key := First(args)
	list := Second(args)
	return Dissv(key, list)
}

func DissocImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	key := First(args)
	list := Second(args)
	return Dissoc(key, list)
}
