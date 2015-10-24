// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file implements some utilities.

package golisp

func ArrayToList(sexprs []*Data) *Data {
	head := Cons(nil, EmptyCons())
	lastCell := head
	for _, element := range sexprs {
		if element == nil {
			element = EmptyCons()
		}
		newCell := Cons(element, nil)
		ConsValue(lastCell).Cdr = newCell
		lastCell = newCell
	}
	return Cdr(head)
}

func ArrayToVectorizedList(sexprs []*Data) *Data {
	return VectorizedListWithValue(sexprs)
}

func ArrayToListWithTail(sexprs []*Data, tail *Data) *Data {
	head := Cons(nil, EmptyCons())
	lastCell := head
	for _, element := range sexprs {
		if element == nil {
			element = EmptyCons()
		}
		newCell := Cons(element, nil)
		ConsValue(lastCell).Cdr = newCell
		lastCell = newCell
	}
	ConsValue(lastCell).Cdr = tail
	return Cdr(head)
}

func ToArray(list *Data) []*Data {
	if VectorizedListP(list) {
		return VectorizedListValue(list)
	} else {
		result := make([]*Data, 0)
		for c := list; NotNilP(c); c = Cdr(c) {
			result = append(result, Car(c))
		}
		return result
	}
}
