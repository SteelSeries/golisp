// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpreter for embedding in a go program for scripting.
// This file contains the association list primitive functions.

package golisp

func RegisterAListPrimitives() {
	MakePrimitiveFunction("acons", "2|3", AconsImpl)
	MakePrimitiveFunction("pairlis", "2|3", PairlisImpl)
	MakePrimitiveFunction("assq", "2", AssocImpl)
	MakePrimitiveFunction("assv", "2", AssocImpl)
	MakePrimitiveFunction("assoc", "2", AssocImpl)
	MakePrimitiveFunction("dissoc", "2", DissocImpl)
	MakePrimitiveFunction("rassoc", "2", RassocImpl)
	MakePrimitiveFunction("alist", "1", AlistImpl)
}

func AlistImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l := Car(args)
	result = Alist(l)
	return
}

func AconsImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	key := First(args)
	if PairP(key) {
		err = ProcessError("Alist key can not be a list", env)
		return
	}

	value := Second(args)

	var alist *Data = nil
	if Length(args) == 3 {
		alist = Third(args)
	}

	return Acons(key, value, alist), nil
}

func PairlisImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	keys := Car(args)
	if !PairP(keys) {
		err = ProcessError("First arg of pairlis must be a list", env)
		return
	}

	values := Cadr(args)

	if !PairP(values) {
		err = ProcessError("Second arg of Pairlis must be a list", env)
		return
	}

	if Length(keys) != Length(values) {
		err = ProcessError("Pairlis requires the same number of keys and values", env)
		return
	}

	result = Third(args)

	if NotNilP(result) {
		if !PairP(result) {
			err = ProcessError("Third arg of pairlis must be an association list (if provided)", env)
			return
		}
	}

	for keyCell, valueCell := keys, values; NotNilP(keyCell); keyCell, valueCell = Cdr(keyCell), Cdr(valueCell) {
		key := Car(keyCell)
		if NilP(key) {
			err = ProcessError("Assoc list keys can not be nil", env)
		}
		value := Car(valueCell)
		result = Acons(key, value, result)
	}

	return
}

func AssocImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	key := Car(args)
	list := Cadr(args)
	return Assoc(key, list)
}

func RassocImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	value := Car(args)
	list := Cadr(args)

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

func DissocImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	key := Car(args)
	list := Cadr(args)
	return Dissoc(key, list)
}
