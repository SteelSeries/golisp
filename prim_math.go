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
	MakePrimitiveFunction("+", "*", AddImpl)
	MakePrimitiveFunction("-", "*", SubtractImpl)
	MakePrimitiveFunction("*", "*", MultiplyImpl)
	MakePrimitiveFunction("/", "*", QuotientImpl)
	MakePrimitiveFunction("quotient", "*", QuotientImpl)
	MakePrimitiveFunction("%", "2", RemainderImpl)
	MakePrimitiveFunction("modulo", "2", RemainderImpl)
	MakePrimitiveFunction("random-byte", "0", RandomByteImpl)
	MakePrimitiveFunction("interval", "1|2|3", IntervalImpl)
	MakePrimitiveFunction("integer", "1", ToIntImpl)
	MakePrimitiveFunction("float", "1", ToFloatImpl)
	MakePrimitiveFunction("number->string", "1|2", NumberToStringImpl)
	MakePrimitiveFunction("string->number", "1|2", StringToNumberImpl)
	MakePrimitiveFunction("min", "1", MinImpl)
	MakePrimitiveFunction("max", "1", MaxImpl)
	MakePrimitiveFunction("floor", "1", FloorImpl)
	MakePrimitiveFunction("ceiling", "1", CeilingImpl)
	MakePrimitiveFunction("abs", "1", AbsImpl)
	MakePrimitiveFunction("zero?", "1", ZeroImpl)
	MakePrimitiveFunction("positive?", "1", PositiveImpl)
	MakePrimitiveFunction("negative?", "1", NegativeImpl)
	MakePrimitiveFunction("even?", "1", EvenImpl)
	MakePrimitiveFunction("odd?", "1", OddImpl)
	MakePrimitiveFunction("sign", "1", SignImpl)
}

func sgn(a float32) int64 {
	switch {
	case a < 0:
		return -1
	case a > 0:
		return +1
	}
	return 0
}

func intSgn(a int64) int64 {
	return sgn(float32(a))
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
	dividend := Car(args)
	if !IntegerP(dividend) {
		err = ProcessError(fmt.Sprintf("%/modulo expected an integer first arg, received %s", String(dividend)), env)
		return
	}

	divisor := Cadr(args)
	if !IntegerP(dividend) {
		err = ProcessError(fmt.Sprintf("%/modulo expected an integer second arg, received %s", String(divisor)), env)
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
	var direction int64 = 1
	var step int64
	var end int64

	startObj := Car(args)
	start := IntegerValue(startObj)

	if Length(args) == 1 {
		direction = 1
		step = 1
		end = start
		start = 1
	} else {

		endObj := Cadr(args)
		end = IntegerValue(endObj)

		if start > end {
			direction = -1
		}

		if Length(args) == 3 {
			if !IntegerP(Caddr(args)) {
				err = ProcessError(fmt.Sprintf("interval step must be an integer, received %s", String(Caddr(args))), env)
				return
			}
			step = IntegerValue(Caddr(args))
			if intSgn(step) != direction {
				return nil, ProcessError("The sign of step has to match the direction of the interval", env)
			}
		} else {
			step = direction
		}
	}
	var items []*Data = make([]*Data, 0, int(math.Abs(float64(end-start)))+1)

	if direction == 1 {
		for i := start; i <= end; i = i + step {
			items = append(items, IntegerWithValue(i))
		}
	} else {
		for i := start; i >= end; i = i + step {
			items = append(items, IntegerWithValue(i))
		}
	}

	result = ArrayToList(items)
	return
}

func ToIntImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	n := Car(args)
	if !NumberP(n) {
		err = ProcessError(fmt.Sprintf("integer expected an number, received %s", String(n)), env)
		return
	}

	return IntegerWithValue(IntegerValue(n)), nil
}

func ToFloatImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	n := Car(args)
	if !NumberP(n) {
		err = ProcessError(fmt.Sprintf("float expected a number, received %s", String(n)), env)
		return
	}

	return FloatWithValue(FloatValue(n)), nil
}

func NumberToStringImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	valObj := First(args)
	val := IntegerValue(valObj)
	var base int64
	if Length(args) == 2 {
		baseObj := Second(args)
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
	strObj := First(args)
	str := StringValue(strObj)
	var base int64
	if Length(args) == 2 {
		baseObj := Second(args)
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
	n := Car(args)
	if !IntegerP(n) {
		err = ProcessError(fmt.Sprintf("min requires numbers, received %s", String(n)), env)
		return
	}
	var acc int64 = IntegerValue(n)

	for c := Cdr(args); NotNilP(c); c = Cdr(c) {
		n = Car(c)
		if !IntegerP(n) {
			err = ProcessError(fmt.Sprintf("min requires numbers, received %s", String(n)), env)
			return
		}
		if IntegerValue(n) < acc {
			acc = IntegerValue(n)
		}
	}

	return IntegerWithValue(acc), nil
}

func minFloats(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	n := Car(args)
	if !NumberP(n) {
		err = ProcessError(fmt.Sprintf("min requires numbers, received %s", String(n)), env)
		return
	}
	var acc float32 = FloatValue(n)

	for c := Cdr(args); NotNilP(c); c = Cdr(c) {
		n = Car(c)
		if !NumberP(n) {
			err = ProcessError(fmt.Sprintf("min requires numbers, received %s", String(n)), env)
			return
		}
		if FloatValue(n) < acc {
			acc = FloatValue(n)
		}
	}

	return FloatWithValue(acc), nil
}

func MinImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	numbers := Car(args)
	if !ListP(numbers) {
		err = ProcessError(fmt.Sprintf("min requires a list of numbers, received %s", String(numbers)), env)
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
	n := Car(args)
	if !IntegerP(n) {
		err = ProcessError(fmt.Sprintf("max requires numbers, received %s", String(n)), env)
		return
	}
	var acc int64 = IntegerValue(n)

	for c := Cdr(args); NotNilP(c); c = Cdr(c) {
		n = Car(c)
		if !IntegerP(n) {
			err = ProcessError(fmt.Sprintf("max requires numbers, received %s", String(n)), env)
			return
		}
		if IntegerValue(n) > acc {
			acc = IntegerValue(n)
		}
	}

	return IntegerWithValue(acc), nil
}

func maxFloats(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	n := Car(args)
	if !NumberP(n) {
		err = ProcessError(fmt.Sprintf("max requires numbers, received %s", String(n)), env)
		return
	}
	var acc float32 = FloatValue(n)

	for c := Cdr(args); NotNilP(c); c = Cdr(c) {
		n = Car(c)
		if !NumberP(n) {
			err = ProcessError(fmt.Sprintf("max requires numbers, received %s", String(n)), env)
			return
		}
		if FloatValue(n) > acc {
			acc = FloatValue(n)
		}
	}

	return FloatWithValue(acc), nil
}

func MaxImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	numbers := Car(args)
	if !ListP(numbers) {
		err = ProcessError(fmt.Sprintf("max requires a list of numbers, received %s", String(numbers)), env)
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
	val := Car(args)

	if !NumberP(val) {
		err = ProcessError(fmt.Sprintf("floor expected an number, received %s", String(Car(args))), env)
		return
	}

	return FloatWithValue(float32(math.Floor(float64(FloatValue(val))))), nil
}

func CeilingImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	val := Car(args)

	if !NumberP(val) {
		err = ProcessError(fmt.Sprintf("ceiling expected a number, received %s", String(Car(args))), env)
		return
	}

	return FloatWithValue(float32(math.Ceil(float64(FloatValue(val))))), nil
}

func AbsImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	val := Car(args)
	if !NumberP(val) {
		err = ProcessError(fmt.Sprintf("abs expected a number, received %s", String(Car(args))), env)
		return
	}
	absval := math.Abs(float64(FloatValue(val)))
	if IntegerP(val) {
		result = IntegerWithValue(int64(absval))
	} else {
		result = FloatWithValue(float32(absval))
	}
	return
}

func ZeroImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	val := Car(args)
	if !NumberP(val) {
		err = ProcessError(fmt.Sprintf("zero? expected a number, received %s", String(Car(args))), env)
		return
	}
	return BooleanWithValue(FloatValue(val) == 0.0), nil
}

func PositiveImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	val := Car(args)
	if !NumberP(val) {
		err = ProcessError(fmt.Sprintf("positive? expected a number, received %s", String(Car(args))), env)
		return
	}
	return BooleanWithValue(FloatValue(val) > 0.0), nil
}

func NegativeImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	val := Car(args)
	if !NumberP(val) {
		err = ProcessError(fmt.Sprintf("negative expected a number, received %s", String(Car(args))), env)
		return
	}
	return BooleanWithValue(FloatValue(val) < 0.0), nil
}

func EvenImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	val := Car(args)
	if !IntegerP(val) {
		err = ProcessError(fmt.Sprintf("even? expected an integer, received %s", String(Car(args))), env)
		return
	}
	return BooleanWithValue(IntegerValue(val)%2 == 0), nil
}

func OddImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	val := Car(args)
	if !IntegerP(val) {
		err = ProcessError(fmt.Sprintf("odd? expected an integer, received %s", String(Car(args))), env)
		return
	}
	return BooleanWithValue(IntegerValue(val)%2 != 0), nil
}

func SignImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	val := Car(args)
	if !NumberP(val) {
		err = ProcessError(fmt.Sprintf("sign expected a nunber, received %s", String(Car(args))), env)
		return
	}

	if FloatP(val) {
		return IntegerWithValue(sgn(float32(FloatValue(val)))), nil
	} else {
		return IntegerWithValue(intSgn(IntegerValue(val))), nil
	}
}
