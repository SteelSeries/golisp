// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the association list primitive functions.

package golisp

import (
	"fmt"
)

func RegisterAListPrimitives() {
	MakePrimitiveFunction("acons", "2|3", AconsImpl)
	MakePrimitiveFunction("pairlis", "2|3", PairlisImpl)
	MakePrimitiveFunction("assoc", "2", AssocImpl)
	MakePrimitiveFunction("dissoc", "2", DissocImpl)
	MakePrimitiveFunction("rassoc", "2", RassocImpl)
}

func AconsImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	key := First(args)
	if ListP(key) {
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
	keys := First(args)
	if !ListP(keys) {
		err = ProcessError("First arg of pairlis must be a list", env)
		return
	}

	values := Second(args)

	if !ListP(values) {
		err = ProcessError("Second arg of pairlis must be a list", env)
		return
	}

	if Length(keys) != Length(values) {
		err = ProcessError("pairlis requires the same number of keys and values", env)
		return
	}

	if Length(args) == 3 {
		result = Third(args)

		if !ListP(result) {
			err = ProcessError("Third arg of pairlis must be an list (if provided)", env)
			return
		}
	}

	for remainingKeys, remainingValues := keys, values; NotNilP(remainingKeys); remainingKeys, remainingValues = Cdr(remainingKeys), Cdr(remainingValues) {
		key := Car(remainingKeys)
		if NilP(key) {
			err = ProcessError("Assoc list keys can not be nil", env)
			return
		}
		value := Car(remainingValues)
		result = Acons(key, value, result)
	}

	return
}

func AssocImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	key := First(args)
	list := Second(args)
	if !ListP(list) {
		err = ProcessError(fmt.Sprintf("assoc require a list as its second arument, but received %s.", String(list)), env)
		return
	}

	return Assoc(key, list)
}

func RassocImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	value := First(args)
	list := Second(args)
	if !ListP(list) {
		err = ProcessError(fmt.Sprintf("rassoc require a list as its second arument, but received %s.", String(list)), env)
		return
	}

	for c := list; NotNilP(c); c = Cdr(c) {
		pair := Car(c)
		if !PairP(pair) {
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
	key := First(args)
	list := Second(args)
	if !ListP(list) {
		err = ProcessError(fmt.Sprintf("dissoc require a list as its second arument, but received %s.", String(list)), env)
		return
	}

	return Dissoc(key, list)
}
