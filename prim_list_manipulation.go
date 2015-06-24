// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the list manipulation primitive functions.

package golisp

import ()

func RegisterListManipulationPrimitives() {
	MakePrimitiveFunction("list", "*", ListImpl)
	MakePrimitiveFunction("make-list", "1|2", MakeListImpl)
	MakePrimitiveFunction("length", "1", ListLengthImpl)
	MakePrimitiveFunction("cons", "2", ConsImpl)
	MakePrimitiveFunction("reverse", "1", ReverseImpl)
	MakePrimitiveFunction("flatten", "1", FlattenImpl)
	MakePrimitiveFunction("flatten*", "1", RecursiveFlattenImpl)
	MakePrimitiveFunction("append", "*", AppendImpl)
	MakeSpecialForm("append!", "2", AppendBangImpl)
	MakePrimitiveFunction("copy", "1", CopyImpl)
	MakePrimitiveFunction("partition", "2", PartitionImpl)
	MakePrimitiveFunction("sublist", "3", SublistImpl)
}

func MakeListImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	kVal := Car(args)
	if !IntegerP(kVal) {
		err = ProcessError("make-list requires a integer as it's first argument.", env)
		return
	}

	k := IntegerValue(kVal)
	var element *Data

	if k < 0 {
		err = ProcessError("make-list requires a non-negative integer as it's first argument.", env)
		return
	}

	if Length(args) == 1 {
		element = nil
	} else {
		element = Cadr(args)
	}

	var items []*Data
	items = make([]*Data, 0, k)
	for ; k > 0; k = k - 1 {
		items = append(items, element)
	}
	return ArrayToList(items), nil
}

func ListImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return args, nil
}

func ListLengthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return IntegerWithValue(int64(Length(Car(args)))), nil
}

func ConsImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	car := Car(args)
	cdr := Cadr(args)
	return Cons(car, cdr), nil
}

func ReverseImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return Reverse(Car(args)), nil
}

func FlattenImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return Flatten(Car(args))
}

func RecursiveFlattenImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return RecursiveFlatten(Car(args))
}

func AppendBangImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	firstList, err := Eval(Car(args), env)
	if err != nil {
		return
	}

	second, err := Eval(Cadr(args), env)
	if err != nil {
		return
	}

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

func partitionBySize(determiner *Data, l *Data, env *SymbolTableFrame) (result *Data, err error) {
	size := int(IntegerValue(determiner))
	if size < 1 {
		err = ProcessError("partition requires a non negative clump size.", env)
		return
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

func partitionByPredicate(determiner *Data, l *Data, env *SymbolTableFrame) (result *Data, err error) {
	pieces := make([]*Data, 0, 5)
	falseSection := make([]*Data, 0, 5)
	trueSection := make([]*Data, 0, 5)
	var predicateResult *Data
	for c := l; NotNilP(c); c = Cdr(c) {
		predicateResult, err = Apply(determiner, InternalMakeList(Car(c)), env)
		if err != nil {
			return
		}
		if BooleanValue(predicateResult) {
			trueSection = append(trueSection, Car(c))
		} else {
			falseSection = append(falseSection, Car(c))
		}
	}

	pieces = append(pieces, ArrayToList(trueSection))
	pieces = append(pieces, ArrayToList(falseSection))

	return ArrayToList(pieces), nil
}

func PartitionImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	determiner := Car(args)
	if !IntegerP(determiner) && !FunctionP(determiner) {
		err = ProcessError("partition requires an integer or function as it's first argument.", env)
		return
	}

	l := Cadr(args)
	if !ListP(l) {
		err = ProcessError("partition requires a list as it's second argument.", env)
		return
	}

	if IntegerP(determiner) {
		return partitionBySize(determiner, l, env)
	} else {
		return partitionByPredicate(determiner, l, env)
	}
}

func SublistImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l := Car(args)
	if !ListP(l) {
		err = ProcessError("sublist requires a list as it's first argument.", env)
		return
	}

	n := Cadr(args)
	if !IntegerP(n) {
		err = ProcessError("sublist requires a number as it's second argument (start).", env)
		return
	}
	first := int(IntegerValue(n))

	if first <= 0 {
		err = ProcessError("sublist requires positive indecies.", env)
		return
	}

	n = Caddr(args)
	if !IntegerP(n) {
		err = ProcessError("sublist requires a number as it's third argument (end).", env)
		return
	}
	last := int(IntegerValue(n))

	if last <= 0 {
		err = ProcessError("sublist requires positive indecies.", env)
		return
	}

	if first >= last {
		return
	}

	var cell *Data
	var i int
	for i, cell = 1, l; i < first && NotNilP(cell); i, cell = i+1, Cdr(cell) {
	}

	var items []*Data = make([]*Data, 0, Length(args))
	for ; i < last && NotNilP(cell); i, cell = i+1, Cdr(cell) {
		items = append(items, Car(cell))
	}
	result = ArrayToList(items)
	return
}
