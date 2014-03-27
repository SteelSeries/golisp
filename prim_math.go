// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the built-in primitive functions.

package golisp

import (
	"errors"
	"fmt"
	"math/rand"
)

func RegisterMathPrimitives() {
	MakePrimitiveFunction("+", -1, AddImpl)
	MakePrimitiveFunction("-", -1, SubtractImpl)
	MakePrimitiveFunction("*", -1, MultiplyImpl)
	MakePrimitiveFunction("/", -1, QuotientImpl)
	MakePrimitiveFunction("%", 2, RemainderImpl)
	MakePrimitiveFunction("modulo", 2, RemainderImpl)
	MakePrimitiveFunction("random-byte", 0, RandomByteImpl)
	MakePrimitiveFunction("interval", 2, IntervalImpl)
	MakePrimitiveFunction("integer", 1, ToIntImpl)
	MakePrimitiveFunction("float", 1, ToFloatImpl)
	MakePrimitiveFunction("number->string", -1, NumberToStringImpl)
}

func IsEvenImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	evaluated, _ := Eval(Car(args), env)
	if !IntegerP(evaluated) {
		return False, nil
	}
	return BooleanWithValue((IntegerValue(evaluated) % 2) == 0), nil
}

func IsOddImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	evaluated, _ := Eval(Car(args), env)
	if !IntegerP(evaluated) {
		return False, nil
	}
	return BooleanWithValue((IntegerValue(evaluated) % 2) == 1), nil
}

func addFloats(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var acc float32 = 0
	var n *Data
	for c := args; NotNilP(c); c = Cdr(c) {
		n, err = Eval(Car(c), env)
		acc += FloatValue(n)
	}
	return FloatWithValue(acc), nil
}

func addInts(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var acc int64 = 0
	var n *Data
	for c := args; NotNilP(c); c = Cdr(c) {
		n, err = Eval(Car(c), env)
		acc += IntegerValue(n)
	}
	return IntegerWithValue(acc), nil
}

func anyFloats(args *Data, env *SymbolTableFrame) (result bool, err error) {
	var n *Data
	for c := args; NotNilP(c); c = Cdr(c) {
		n, err = Eval(Car(c), env)
		if err != nil {
			return
		} else if !IntegerP(n) && !FloatP(n) {
			err = errors.New(fmt.Sprintf("Number expected, received %s", String(n)))
			return
		}
		if FloatP(n) {
			return true, nil
		}
	}
	return false, nil
}

func AddImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	areFloats, err := anyFloats(args, env)
	if err != nil {
		return
	}
	if areFloats {
		return addFloats(args, env)
	} else {
		return addInts(args, env)
	}
}

func subtractInts(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var n *Data
	n, err = Eval(Car(args), env)
	if err != nil {
		return
	}
	acc := IntegerValue(n)
	for c := Cdr(args); NotNilP(c); c = Cdr(c) {
		n, err = Eval(Car(c), env)
		if err != nil {
			return
		}
		acc -= IntegerValue(n)
	}
	return IntegerWithValue(acc), nil
}

func subtractFloats(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var n *Data
	n, err = Eval(Car(args), env)
	if err != nil {
		return
	}
	acc := FloatValue(n)
	for c := Cdr(args); NotNilP(c); c = Cdr(c) {
		n, err = Eval(Car(c), env)
		if err != nil {
			return
		}
		acc -= FloatValue(n)
	}
	return FloatWithValue(acc), nil
}

func SubtractImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	areFloats, err := anyFloats(args, env)
	if err != nil {
		return
	}
	if areFloats {
		return subtractFloats(args, env)
	} else {
		return subtractInts(args, env)
	}
}

func multiplyInts(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var n *Data
	var acc int64 = 1
	for c := args; NotNilP(c); c = Cdr(c) {
		n, err = Eval(Car(c), env)
		if err != nil {
			return
		}
		acc *= IntegerValue(n)
	}
	return IntegerWithValue(acc), nil
}

func multiplyFloats(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var n *Data
	var acc float32 = 1.0
	for c := args; NotNilP(c); c = Cdr(c) {
		n, err = Eval(Car(c), env)
		if err != nil {
			return
		}
		acc *= FloatValue(n)
	}
	return FloatWithValue(acc), nil
}

func MultiplyImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	areFloats, err := anyFloats(args, env)
	if err != nil {
		return
	}
	if areFloats {
		return multiplyFloats(args, env)
	} else {
		return multiplyInts(args, env)
	}
}

func quotientInts(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var n *Data
	n, err = Eval(Car(args), env)
	if err != nil {
		return
	}
	acc := IntegerValue(n)
	for c := Cdr(args); NotNilP(c); c = Cdr(c) {
		n, err = Eval(Car(c), env)
		if err != nil {
			return
		}
		v := IntegerValue(n)
		if v == 0 {
			errors.New(fmt.Sprintf("Quotent: %s -> Divide by zero.", String(args)))
		} else {
			acc /= IntegerValue(n)
		}
	}
	return IntegerWithValue(acc), nil
}

func quotientFloats(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var n *Data
	n, err = Eval(Car(args), env)
	if err != nil {
		return
	}
	var acc float32 = FloatValue(n)
	for c := Cdr(args); NotNilP(c); c = Cdr(c) {
		n, err = Eval(Car(c), env)
		if err != nil {
			return
		}
		acc /= FloatValue(n)
	}
	return FloatWithValue(acc), nil
}

func QuotientImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	areFloats, err := anyFloats(args, env)
	if err != nil {
		return
	}
	if areFloats {
		return quotientFloats(args, env)
	} else {
		return quotientInts(args, env)
	}
}

func RemainderImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if Length(args) != 2 {
		err = errors.New(fmt.Sprintf("2 args expected, %d received", Length(args)))
		return
	}

	var dividend *Data
	dividend, err = Eval(Car(args), env)
	if err != nil {
		return
	}
	if !IntegerP(dividend) {
		err = errors.New(fmt.Sprintf("Number expected, received %s", String(dividend)))
		return
	}

	var divisor *Data
	divisor, err = Eval(Cadr(args), env)
	if err != nil {
		return
	}
	if !IntegerP(dividend) {
		err = errors.New(fmt.Sprintf("Number expected, received %s", String(divisor)))
		return
	}

	val := IntegerValue(dividend) % IntegerValue(divisor)
	return IntegerWithValue(val), nil
}

func RandomByteImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	r := uint8(rand.Int())
	result = IntegerWithValue(int64(r))
	return
}

func IntervalImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	startObj, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	start := IntegerValue(startObj)

	endObj, err := Eval(Cadr(args), env)
	if err != nil {
		return
	}
	end := IntegerValue(endObj)

	var items []*Data = make([]*Data, 0, end-start+1)

	for i := start; i <= end; i = i + 1 {
		items = append(items, IntegerWithValue(i))
	}
	result = ArrayToList(items)
	return
}

func ToIntImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	n, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !IntegerP(n) && !FloatP(n) {
		err = errors.New(fmt.Sprintf("Number expected, received %s", String(n)))
		return
	}

	return IntegerWithValue(IntegerValue(n)), nil
}

func ToFloatImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	n, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !IntegerP(n) && !FloatP(n) {
		err = errors.New(fmt.Sprintf("Number expected, received %s", String(n)))
		return
	}

	return FloatWithValue(FloatValue(n)), nil
}

func NumberToStringImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	val := IntegerValue(First(args))
	var base int64
	if Length(args) == 2 {
		base = IntegerValue(Second(args))
	} else {
		base = 10
	}

	var format string
	switch base {
	case 2:
		format = "%b"
	case 8:
		format = "%o"
	case 10:
		format = "%d"
	case 16:
		format = "%x"
	default:
		format = "Unsupported base: %d"
		val = base
	}
	return StringWithValue(fmt.Sprintf(format, val)), nil
}
