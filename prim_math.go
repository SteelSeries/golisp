// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the built-in primitive functions.

package golisp

import (
	"fmt"
	"math"
	"math/rand"
)

func RegisterMathPrimitives() {
	MakePrimitiveFunction("+", -1, AddImpl)
	MakePrimitiveFunction("-", -1, SubtractImpl)
	MakePrimitiveFunction("*", -1, MultiplyImpl)
	MakePrimitiveFunction("/", -1, QuotientImpl)
	MakePrimitiveFunction("quotient", -1, QuotientImpl)
	MakePrimitiveFunction("%", 2, RemainderImpl)
	MakePrimitiveFunction("modulo", 2, RemainderImpl)
	MakePrimitiveFunction("random-byte", 0, RandomByteImpl)
	MakePrimitiveFunction("interval", -1, IntervalImpl)
	MakePrimitiveFunction("integer", 1, ToIntImpl)
	MakePrimitiveFunction("float", 1, ToFloatImpl)
	MakePrimitiveFunction("number->string", -1, NumberToStringImpl)
	MakePrimitiveFunction("string->number", -1, StringToNumberImpl)
	MakePrimitiveFunction("min", 1, MinImpl)
	MakePrimitiveFunction("max", 1, MaxImpl)
	MakePrimitiveFunction("floor", 1, FloorImpl)
	MakePrimitiveFunction("ceiling", 1, CeilingImpl)

}

func IsEvenImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	evaluated, _ := Eval(Car(args), env)
	if !IntegerP(evaluated) {
		return LispFalse, nil
	}
	return BooleanWithValue((IntegerValue(evaluated) % 2) == 0), nil
}

func IsOddImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	evaluated, _ := Eval(Car(args), env)
	if !IntegerP(evaluated) {
		return LispFalse, nil
	}
	return BooleanWithValue((IntegerValue(evaluated) % 2) == 1), nil
}

func addFloats(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var acc float32 = 0
	for c := args; NotNilP(c); c = Cdr(c) {
		acc += FloatValue(Car(c))
	}
	return FloatWithValue(acc), nil
}

func addInts(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var acc int64 = 0
	for c := args; NotNilP(c); c = Cdr(c) {
		acc += IntegerValue(Car(c))
	}
	return IntegerWithValue(acc), nil
}

func anyFloats(args *Data, env *SymbolTableFrame) (result bool, err error) {
	for c := args; NotNilP(c); c = Cdr(c) {
		if !NumberP(Car(c)) {
			err = ProcessError(fmt.Sprintf("Number expected, received %s", String(Car(c))), env)
			return
		}
		if FloatP(Car(c)) {
			return true, nil
		}
	}
	return false, nil
}

func AddImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var n *Data
	nums := make([]*Data, 0, Length(args))
	for c := args; NotNilP(c); c = Cdr(c) {
		n, err = Eval(Car(c), env)
		if err != nil {
			return
		}
		nums = append(nums, n)
	}
	numbers := ArrayToList(nums)
	areFloats, err := anyFloats(numbers, env)
	if err != nil {
		return
	}
	if areFloats {
		return addFloats(numbers, env)
	} else {
		return addInts(numbers, env)
	}
}

func subtractInts(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	acc := IntegerValue(Car(args))
	for c := Cdr(args); NotNilP(c); c = Cdr(c) {
		acc -= IntegerValue(Car(c))
	}
	return IntegerWithValue(acc), nil
}

func subtractFloats(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	acc := FloatValue(Car(args))
	for c := Cdr(args); NotNilP(c); c = Cdr(c) {
		acc -= FloatValue(Car(c))
	}
	return FloatWithValue(acc), nil
}

func SubtractImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var n *Data
	nums := make([]*Data, 0, Length(args))
	for c := args; NotNilP(c); c = Cdr(c) {
		n, err = Eval(Car(c), env)
		if err != nil {
			return
		}
		nums = append(nums, n)
	}
	numbers := ArrayToList(nums)
	areFloats, err := anyFloats(numbers, env)
	if err != nil {
		return
	}
	if areFloats {
		return subtractFloats(numbers, env)
	} else {
		return subtractInts(numbers, env)
	}
}

func multiplyInts(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var acc int64 = 1
	for c := args; NotNilP(c); c = Cdr(c) {
		acc *= IntegerValue(Car(c))
	}
	return IntegerWithValue(acc), nil
}

func multiplyFloats(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var acc float32 = 1.0
	for c := args; NotNilP(c); c = Cdr(c) {
		acc *= FloatValue(Car(c))
	}
	return FloatWithValue(acc), nil
}

func MultiplyImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var n *Data
	nums := make([]*Data, 0, Length(args))
	for c := args; NotNilP(c); c = Cdr(c) {
		n, err = Eval(Car(c), env)
		if err != nil {
			return
		}
		nums = append(nums, n)
	}
	numbers := ArrayToList(nums)
	areFloats, err := anyFloats(numbers, env)
	if err != nil {
		return
	}
	if areFloats {
		return multiplyFloats(numbers, env)
	} else {
		return multiplyInts(numbers, env)
	}
}

func quotientInts(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	acc := IntegerValue(Car(args))
	for c := Cdr(args); NotNilP(c); c = Cdr(c) {
		v := IntegerValue(Car(c))
		if v == 0 {
			err = ProcessError(fmt.Sprintf("Quotent: %s -> Divide by zero.", String(args)), env)
			return
		} else {
			acc /= v
		}
	}
	return IntegerWithValue(acc), nil
}

func quotientFloats(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var acc float32 = FloatValue(Car(args))
	for c := Cdr(args); NotNilP(c); c = Cdr(c) {
		v := FloatValue(Car(c))
		if v == 0 {
			err = ProcessError(fmt.Sprintf("Quotent: %s -> Divide by zero.", String(args)), env)
			return
		} else {
			acc /= v
		}
	}
	return FloatWithValue(acc), nil
}

func QuotientImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	nums := make([]*Data, 0, Length(args))
	var n *Data
	for c := args; NotNilP(c); c = Cdr(c) {
		n, err = Eval(Car(c), env)
		if err != nil {
			return
		}
		nums = append(nums, n)
	}
	numbers := ArrayToList(nums)

	areFloats, err := anyFloats(numbers, env)
	if err != nil {
		return
	}
	if areFloats {
		return quotientFloats(numbers, env)
	} else {
		return quotientInts(numbers, env)
	}
}

func RemainderImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if Length(args) != 2 {
		err = ProcessError(fmt.Sprintf("2 args expected, %d received", Length(args)), env)
		return
	}

	var dividend *Data
	dividend, err = Eval(Car(args), env)
	if err != nil {
		return
	}
	if !IntegerP(dividend) {
		err = ProcessError(fmt.Sprintf("Number expected, received %s", String(dividend)), env)
		return
	}

	var divisor *Data
	divisor, err = Eval(Cadr(args), env)
	if err != nil {
		return
	}
	if !IntegerP(dividend) {
		err = ProcessError(fmt.Sprintf("Number expected, received %s", String(divisor)), env)
		return
	}

	val := IntegerValue(dividend) % IntegerValue(divisor)
	return IntegerWithValue(val), nil
}

// Not tested since it just wraps rand.Int()
func RandomByteImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	r := uint8(rand.Int())
	result = IntegerWithValue(int64(r))
	return
}

func IntervalImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if Length(args) < 2 || Length(args) > 3 {
		err = ProcessError(fmt.Sprintf("interval expects 2 or 3 arguments, received %d", Length(args)), env)
		return
	}

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

	var step int64 = 1
	if Length(args) == 3 {
		stepObj, e := Eval(Caddr(args), env)
		if e != nil {
			return nil, e
		}
		step = IntegerValue(stepObj)
		if step < 1 {
			return nil, ProcessError(fmt.Sprintf("interval expects a positive step value, received %d", step), env)
		}
	}

	var items []*Data = make([]*Data, 0, end-start+1)

	for i := start; i <= end; i = i + step {
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
		err = ProcessError(fmt.Sprintf("Number expected, received %s", String(n)), env)
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
		err = ProcessError(fmt.Sprintf("Number expected, received %s", String(n)), env)
		return
	}

	return FloatWithValue(FloatValue(n)), nil
}

func NumberToStringImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	valObj, err := Eval(First(args), env)
	if err != nil {
		return nil, err
	}
	val := IntegerValue(valObj)
	var base int64
	if Length(args) == 2 {
		baseObj, err := Eval(Second(args), env)
		if err != nil {
			return nil, err
		}
		base = IntegerValue(baseObj)
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

func StringToNumberImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	strObj, err := Eval(First(args), env)
	if err != nil {
		return nil, err
	}
	str := StringValue(strObj)
	var base int64
	if Length(args) == 2 {
		baseObj, err := Eval(Second(args), env)
		if err != nil {
			return nil, err
		}
		base = IntegerValue(baseObj)
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
		return IntegerWithValue(0), nil
	}
	var val int64
	_, err = fmt.Sscanf(str, format, &val)
	if err != nil {
		return
	}
	return IntegerWithValue(val), nil
}

func minInts(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var n *Data
	n, err = Eval(Car(args), env)
	if err != nil {
		return
	} else if !IntegerP(n) {
		err = ProcessError(fmt.Sprintf("Min requires numbers, received %s", String(n)), env)
		return
	}
	var acc int64 = IntegerValue(n)

	for c := Cdr(args); NotNilP(c); c = Cdr(c) {
		n, err = Eval(Car(c), env)
		if err != nil {
			return
		} else if !IntegerP(n) {
			err = ProcessError(fmt.Sprintf("Min requires numbers, received %s", String(n)), env)
			return
		}
		if IntegerValue(n) < acc {
			acc = IntegerValue(n)
		}
	}

	return IntegerWithValue(acc), nil
}

func minFloats(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var n *Data
	n, err = Eval(Car(args), env)
	if err != nil {
		return
	} else if !NumberP(n) {
		err = ProcessError(fmt.Sprintf("Min requires numbers, received %s", String(n)), env)
		return
	}
	var acc float32 = FloatValue(n)

	for c := Cdr(args); NotNilP(c); c = Cdr(c) {
		n, err = Eval(Car(c), env)
		if err != nil {
			return
		} else if !NumberP(n) {
			err = ProcessError(fmt.Sprintf("Min requires numbers, received %s", String(n)), env)
			return
		}
		if FloatValue(n) < acc {
			acc = FloatValue(n)
		}
	}

	return FloatWithValue(acc), nil
}

func MinImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {

	numbers, err := Eval(Car(args), env)
	if !ListP(numbers) {
		err = ProcessError(fmt.Sprintf("Min requires a list of numbers, received %s", String(numbers)), env)
		return
	}
	if Length(numbers) == 0 {
		return IntegerWithValue(0), nil
	}

	areFloats, err := anyFloats(numbers, env)
	if err != nil {
		return
	}
	if areFloats {
		return minFloats(numbers, env)
	} else {
		return minInts(numbers, env)
	}
}

func maxInts(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var n *Data
	n, err = Eval(Car(args), env)
	if err != nil {
		return
	} else if !IntegerP(n) {
		err = ProcessError(fmt.Sprintf("Max requires numbers, received %s", String(n)), env)
		return
	}
	var acc int64 = IntegerValue(n)

	for c := Cdr(args); NotNilP(c); c = Cdr(c) {
		n, err = Eval(Car(c), env)
		if err != nil {
			return
		} else if !IntegerP(n) {
			err = ProcessError(fmt.Sprintf("Max requires numbers, received %s", String(n)), env)
			return
		}
		if IntegerValue(n) > acc {
			acc = IntegerValue(n)
		}
	}

	return IntegerWithValue(acc), nil
}

func maxFloats(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var n *Data
	n, err = Eval(Car(args), env)
	if err != nil {
		return
	} else if !NumberP(n) {
		err = ProcessError(fmt.Sprintf("Max requires numbers, received %s", String(n)), env)
		return
	}
	var acc float32 = FloatValue(n)

	for c := Cdr(args); NotNilP(c); c = Cdr(c) {
		n, err = Eval(Car(c), env)
		if err != nil {
			return
		} else if !NumberP(n) {
			err = ProcessError(fmt.Sprintf("Max requires numbers, received %s", String(n)), env)
			return
		}
		if FloatValue(n) > acc {
			acc = FloatValue(n)
		}
	}

	return FloatWithValue(acc), nil
}

func MaxImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {

	numbers, err := Eval(Car(args), env)
	if !ListP(numbers) {
		err = ProcessError(fmt.Sprintf("Max requires a list of numbers, received %s", String(numbers)), env)
		return
	}

	if Length(numbers) == 0 {
		return IntegerWithValue(0), nil
	}

	areFloats, err := anyFloats(numbers, env)
	if err != nil {
		return
	}
	if areFloats {
		return maxFloats(numbers, env)
	} else {
		return maxInts(numbers, env)
	}
}

func FloorImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	val, err := Eval(Car(args), env)

	if !NumberP(val) {
		err = ProcessError(fmt.Sprintf("Number expected, received %s", String(Car(args))), env)
		return
	}

	return FloatWithValue(float32(math.Floor(float64(FloatValue(val))))), nil
}

func CeilingImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	val, err := Eval(Car(args), env)

	if !NumberP(val) {
		err = ProcessError(fmt.Sprintf("Number expected, received %s", String(Car(args))), env)
		return
	}

	return FloatWithValue(float32(math.Ceil(float64(FloatValue(val))))), nil
}
