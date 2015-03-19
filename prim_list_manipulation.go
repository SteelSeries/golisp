// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the list manipulation primitive functions.

package golisp

import ()

func RegisterListManipulationPrimitives() {
	MakePrimitiveFunction("list", -1, MakeListImpl)
	MakePrimitiveFunction("length", 1, ListLengthImpl)
	MakePrimitiveFunction("cons", 2, ConsImpl)
	MakePrimitiveFunction("reverse", 1, ReverseImpl)
	MakePrimitiveFunction("flatten", 1, FlattenImpl)
	MakePrimitiveFunction("flatten*", 1, RecursiveFlattenImpl)
	MakePrimitiveFunction("append", -1, AppendImpl)
	MakePrimitiveFunction("append!", 2, AppendBangImpl)
	MakePrimitiveFunction("copy", 1, CopyImpl)
	MakePrimitiveFunction("partition", 2, PartitionImpl)
	MakePrimitiveFunction("sublist", 3, SublistImpl)
}

func MakeListImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var items []*Data = make([]*Data, 0, Length(args))
	for cell := args; NotNilP(cell); cell = Cdr(cell) {
		items = append(items, Car(cell))
	}
	result = ArrayToList(items)
	return
}

func ListLengthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return IntegerWithValue(int64(Length(Car(args)))), nil
}

func ConsImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	car := Car(args)
	cdr = Cadr(args)
	return Cons(car, cdr), nil
}

func ReverseImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return Reverse(Car(args))
}

func FlattenImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return Flatten(Car(args))
}

func RecursiveFlattenImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return = RecursiveFlatten(Car(args))
}

func AppendBangImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	firstList := Car(args)

	second := Cadr(args)

	if ListP(second) {
		result = AppendBangList(firstList, second)
	} else {
		result = AppendBang(firstList, second)
	}

	if SymbolP(Car(args)) {
		env.BindTo(Car(args), result)
	}

	return
}

func AppendImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	// No args -> empty list
	if Length(args) == 0 {
		return
	}

	// step through args, accumulating elements
	var items []*Data = make([]*Data, 0, 10)
	var item *Data
	for cell := args; NotNilP(cell); cell = Cdr(cell) {
		item = Car(cell)
		if ListP(item) {
			for itemCell := item; NotNilP(itemCell); itemCell = Cdr(itemCell) {
				items = append(items, Car(itemCell))
			}
		} else {
			items = append(items, item)
		}
	}
	result = ArrayToList(items)
	return
}

func CopyImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return Copy(Car(args)), nil
}

func PartitionImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	n := Car(args)
	if !IntegerP(n) {
		err = ProcessError("partition requires a number as it's first argument.", env)
	}
	size := int(IntegerValue(n))

	l := Cadr(args)
	if !ListP(l) {
		err = ProcessError("partition requires a list as it's second argument.", env)
	}

	var pieces []*Data = make([]*Data, 0, 5)
	var chunk []*Data = make([]*Data, 0, 5)
	for c := l; NotNilP(c); c = Cdr(c) {
		if len(chunk) < size {
			chunk = append(chunk, Car(c))
		} else {
			pieces = append(pieces, ArrayToList(chunk))
			chunk = make([]*Data, 0, 5)
			chunk = append(chunk, Car(c))
		}
	}
	if len(chunk) > 0 {
		pieces = append(pieces, ArrayToList(chunk))
	}

	return ArrayToList(pieces), nil
}

func SublistImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	n := First(args)
	if !IntegerP(n) {
		err = ProcessError("sublist requires a number as it's first argument.", env)
	}
	first := int(IntegerValue(n))

	n = Second(args)
	if !IntegerP(n) {
		err = ProcessError("sublist requires a number as it's second argument.", env)
	}
	last := int(IntegerValue(n))

	if first >= last {
		result = nil
		return
	}

	l := Third(args)
	if !ListP(l) {
		err = ProcessError("sublist requires a list as it's third argument.", env)
	}

	var cell *Data
	var i int
	for i, cell = 1, l; i < first && NotNilP(cell); i, cell = i+1, Cdr(cell) {
	}

	var items []*Data = make([]*Data, 0, Length(args))
	for ; i <= last && NotNilP(cell); i, cell = i+1, Cdr(cell) {
		items = append(items, Car(cell))
	}
	result = ArrayToList(items)
	return
}
