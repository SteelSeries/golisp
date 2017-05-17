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
	MakeTypedPrimitiveFunction("+", "*", AddImpl, []uint32{IntegerType | FloatType})
	MakeTypedPrimitiveFunction("-", "*", SubtractImpl, []uint32{IntegerType | FloatType})
	MakeTypedPrimitiveFunction("*", "*", MultiplyImpl, []uint32{IntegerType | FloatType})
	MakeTypedPrimitiveFunction("/", "*", DivideImpl, []uint32{IntegerType | FloatType})
	MakeTypedPrimitiveFunction("succ", "1", IncrementImpl, []uint32{IntegerType})
	MakeTypedPrimitiveFunction("1+", "1", IncrementImpl, []uint32{IntegerType})
	MakeTypedPrimitiveFunction("pred", "1", DecrementImpl, []uint32{IntegerType})
	MakeTypedPrimitiveFunction("-1+", "1", DecrementImpl, []uint32{IntegerType})
	MakeTypedPrimitiveFunction("quotient", "2", QuotientImpl, []uint32{IntegerType, IntegerType})
	MakeTypedPrimitiveFunction("modulo", "2", ModuloImpl, []uint32{IntegerType, IntegerType})
	MakeTypedPrimitiveFunction("remainder", "2", RemainderImpl, []uint32{IntegerType, IntegerType})
	MakeTypedPrimitiveFunction("%", "2", RemainderImpl, []uint32{IntegerType, IntegerType})
	MakePrimitiveFunction("random-byte", "0", RandomByteImpl)
	MakeTypedPrimitiveFunction("random", "0|1", RandomImpl, []uint32{IntegerType | FloatType})
	MakeTypedPrimitiveFunction("interval", "1|2|3", IntervalImpl, []uint32{IntegerType, IntegerType, IntegerType})
	MakeTypedPrimitiveFunction("integer", "1", ToIntImpl, []uint32{IntegerType | FloatType})
	MakeTypedPrimitiveFunction("float", "1", ToFloatImpl, []uint32{IntegerType | FloatType})
	MakeTypedPrimitiveFunction("number->string", "1|2", NumberToStringImpl, []uint32{IntegerType, IntegerType})
	MakeTypedPrimitiveFunction("string->number", "1|2", StringToNumberImpl, []uint32{StringType, IntegerType})
	MakeTypedPrimitiveFunction("min", "1", MinImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("max", "1", MaxImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("floor", "1", FloorImpl, []uint32{IntegerType | FloatType})
	MakeTypedPrimitiveFunction("ceiling", "1", CeilingImpl, []uint32{IntegerType | FloatType})
	MakeTypedPrimitiveFunction("abs", "1", AbsImpl, []uint32{IntegerType | FloatType})
	MakeTypedPrimitiveFunction("zero?", "1", ZeroImpl, []uint32{IntegerType | FloatType})
	MakeTypedPrimitiveFunction("positive?", "1", PositiveImpl, []uint32{IntegerType | FloatType})
	MakeTypedPrimitiveFunction("negative?", "1", NegativeImpl, []uint32{IntegerType | FloatType})
	MakeTypedPrimitiveFunction("even?", "1", EvenImpl, []uint32{IntegerType})
	MakeTypedPrimitiveFunction("odd?", "1", OddImpl, []uint32{IntegerType})
	MakeTypedPrimitiveFunction("sign", "1", SignImpl, []uint32{IntegerType | FloatType})
	MakeTypedPrimitiveFunction("log", "1", LogImpl, []uint32{IntegerType, FloatType})
	MakeTypedPrimitiveFunction("expt", "2", ExptImpl, []uint32{IntegerType, FloatType})
	MakeTypedPrimitiveFunction("pow", "2", PowImpl, []uint32{IntegerType | FloatType, IntegerType | FloatType})
	MakeTypedPrimitiveFunction("inf?", "1", IsInfImpl, []uint32{IntegerType | FloatType, IntegerType | FloatType})
	MakeTypedPrimitiveFunction("nan?", "1", IsNaNImpl, []uint32{IntegerType, FloatType})
	MakeTypedPrimitiveFunction("float->bits", "1", FloatToBitsImpl, []uint32{FloatType})
	MakeTypedPrimitiveFunction("bits->float", "1", BitsToFloatImpl, []uint32{IntegerType})
	MakeTypedPrimitiveFunction("double->bits", "1", DoubleToBitsImpl, []uint32{FloatType})
	MakeTypedPrimitiveFunction("bits->double", "1", BitsToDoubleImpl, []uint32{IntegerType})

	makeUnaryFloatFunction("acos", math.Acos)
	makeUnaryFloatFunction("acosh", math.Acosh)
	makeUnaryFloatFunction("asin", math.Asin)
	makeUnaryFloatFunction("asinh", math.Asinh)
	makeUnaryFloatFunction("atan", math.Atan)
	makeUnaryFloatFunction("atanh", math.Atanh)
	makeUnaryFloatFunction("cbrt", math.Cbrt)
	makeUnaryFloatFunction("cos", math.Cos)
	makeUnaryFloatFunction("cosh", math.Cosh)
	makeUnaryFloatFunction("sin", math.Sin)
	makeUnaryFloatFunction("sinh", math.Sinh)
	makeUnaryFloatFunction("sqrt", math.Sqrt)
	makeUnaryFloatFunction("tan", math.Tan)
	makeUnaryFloatFunction("tanh", math.Tanh)
	makeUnaryFloatFunction("log", math.Log)

	Global.BindToProtected(Intern("pi"), FloatWithValue(math.Pi))
	Global.BindToProtected(Intern("e"), FloatWithValue(math.E))
	Global.BindToProtected(Intern("phi"), FloatWithValue(math.Phi))
	Global.BindToProtected(Intern("sqrt2"), FloatWithValue(math.Sqrt2))
	Global.BindToProtected(Intern("sqrte"), FloatWithValue(math.SqrtE))
	Global.BindToProtected(Intern("sqrtpi"), FloatWithValue(math.SqrtPi))
	Global.BindToProtected(Intern("sqrtphi"), FloatWithValue(math.SqrtPhi))
	Global.BindToProtected(Intern("ln2"), FloatWithValue(math.Ln2))
	Global.BindToProtected(Intern("log2e"), FloatWithValue(math.Log2E))
	Global.BindToProtected(Intern("ln10"), FloatWithValue(math.Ln10))
	Global.BindToProtected(Intern("log10e"), FloatWithValue(math.Log10E))
	Global.BindToProtected(Intern("nan"), FloatWithValue(math.NaN()))
	Global.BindToProtected(Intern("+inf"), FloatWithValue(math.Inf(1)))
	Global.BindToProtected(Intern("-inf"), FloatWithValue(math.Inf(-1)))
}

func makeUnaryFloatFunction(name string, f func(float64) float64) {
	primFunc := func(args *Data, env *SymbolTableFrame) (result *Data, err error) {
		valObj := Car(args)
		if !NumberP(valObj) {
			err = ProcessError(fmt.Sprintf("%s expects a number as a parameter, got %s", name, String(valObj)), env)
			return
		}
		val := FloatValue(valObj)
		ret := f(float64(val))
		return FloatWithValue(ret), nil
	}
	MakeTypedPrimitiveFunction(name, "1", primFunc, []uint32{IntegerType | FloatType})
}

func sgn(a float64) int64 {
	switch {
	case a < 0:
		return -1
	case a > 0:
		return +1
	}
	return 0
}

func intSgn(a int64) int64 {
	return sgn(float64(a))
}

func IncrementImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	val := IntegerValue(First(args))
	return IntegerWithValue(val + 1), nil
}

func DecrementImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	val := IntegerValue(First(args))
	return IntegerWithValue(val - 1), nil
}

func addFloats(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var acc float64 = 0
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
	if Length(args) == 1 {
		acc = -1 * acc
	} else {
		for c := Cdr(args); NotNilP(c); c = Cdr(c) {
			acc -= IntegerValue(Car(c))
		}
	}
	return IntegerWithValue(acc), nil
}

func subtractFloats(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	acc := FloatValue(Car(args))
	if Length(args) == 1 {
		acc = -1.0 * acc
	} else {
		for c := Cdr(args); NotNilP(c); c = Cdr(c) {
			acc -= FloatValue(Car(c))
		}
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
	var acc float64 = 1.0
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

// TODO roll this into DivideImpl
func divideFloats(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var acc float64
	if Length(args) == 1 {
		v := FloatValue(First(args))
		if v == 0.0 {
			err = ProcessError(fmt.Sprintf("Quotient: %s -> Divide by zero.", String(args)), env)
			return
		}
		acc = 1.0 / v
	} else {
		acc = FloatValue(First(args))
		for c := Cdr(args); NotNilP(c); c = Cdr(c) {
			v := FloatValue(Car(c))
			if v == 0.0 {
				err = ProcessError(fmt.Sprintf("Quotent: %s -> Divide by zero.", String(args)), env)
				return
			} else {
				acc /= v
			}
		}
	}
	return FloatWithValue(acc), nil
}

func DivideImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	areFloats, err := anyFloats(args, env)
	q, err := divideFloats(args, env)
	if err != nil {
		return
	}

	if areFloats {
		return q, nil
	} else {
		return IntegerWithValue(IntegerValue(q)), nil
	}
}

func QuotientImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	dividendObj := First(args)
	dividend := IntegerValue(dividendObj)

	divisorObj := Second(args)
	divisor := IntegerValue(divisorObj)

	if divisor == 0 {
		err = ProcessError("quotient: Divide by zero.", env)
		return
	}

	val := dividend / divisor

	if intSgn(val) != intSgn(divisor) {
		val = val * -1
	}

	return IntegerWithValue(val), nil
}

func ModuloImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	dividendObj := First(args)
	dividend := IntegerValue(dividendObj)

	divisorObj := Second(args)
	divisor := IntegerValue(divisorObj)

	if divisor == 0 {
		err = ProcessError("modulo: Divide by zero.", env)
		return
	}

	val := dividend % divisor
	if intSgn(val) != intSgn(divisor) {
		val = val + divisor
	}

	return IntegerWithValue(val), nil
}

func RemainderImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	dividendObj := First(args)
	dividend := IntegerValue(dividendObj)

	divisorObj := Second(args)
	divisor := IntegerValue(divisorObj)

	if divisor == 0 {
		err = ProcessError("remainder: Divide by zero.", env)
		return
	}

	val := dividend % divisor

	if intSgn(val) != intSgn(dividend) {
		val = val * -1
	}

	return IntegerWithValue(val), nil
}

// Not tested since it just wraps rand.Int()
func RandomByteImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	r := uint8(rand.Int())
	result = IntegerWithValue(int64(r))
	return
}

func RandomImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if Length(args) == 0 {
		result = IntegerWithValue(rand.Int63n(math.MaxUint32))
	} else {
		modulus := First(args)
		if IntegerP(modulus) {
			result = IntegerWithValue(rand.Int63n(IntegerValue(modulus)))
		} else if FloatP(modulus) {
			if FloatValue(modulus) != 1.0 {
				err = ProcessError(fmt.Sprintf("random only accepts floating point modulus of 1.0, received %s", String(modulus)), env)
				return
			}
			result = FloatWithValue(rand.Float64())
		} else {
			err = ProcessError(fmt.Sprintf("random expected an integer or float modulus, received %s", String(modulus)), env)
			return
		}
	}
	return
}

func IntervalImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var direction int64 = 1
	var step int64
	var end int64

	start := IntegerValue(First(args))

	if Length(args) == 1 {
		direction = 1
		step = 1
		end = start
		start = 1
	} else {
		end = IntegerValue(Second(args))

		if start > end {
			direction = -1
		}

		if Length(args) == 3 {
			step = IntegerValue(Third(args))
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
	return IntegerWithValue(IntegerValue(First(args))), nil
}

func ToFloatImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return FloatWithValue(FloatValue(First(args))), nil
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
		err = ProcessError(fmt.Sprintf("number->string: unsupported base %d", base), env)
		return
	}
	return StringWithValue(fmt.Sprintf(format, val)), nil
}

func StringToNumberImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	str := StringValue(First(args))
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
		err = ProcessError(fmt.Sprintf("string->number: unsupported base %d", base), env)
		return
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
	var acc float64 = FloatValue(n)

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
	numbers := First(args)
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
	var acc float64 = FloatValue(n)

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
	numbers := First(args)
	if !ListP(numbers) {
		err = ProcessError(fmt.Sprintf("max requires a proper list of numbers, received %s", String(numbers)), env)
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
	return FloatWithValue(math.Floor(float64(FloatValue(First(args))))), nil
}

func CeilingImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return FloatWithValue(math.Ceil(float64(FloatValue(First(args))))), nil
}

func AbsImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	val := First(args)
	absval := math.Abs(FloatValue(val))
	if IntegerP(val) {
		result = IntegerWithValue(int64(absval))
	} else {
		result = FloatWithValue(absval)
	}
	return
}

func ZeroImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	val := Car(args)
	return BooleanWithValue(FloatValue(val) == 0.0), nil
}

func PositiveImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	val := Car(args)
	return BooleanWithValue(FloatValue(val) > 0.0), nil
}

func NegativeImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	val := Car(args)
	return BooleanWithValue(FloatValue(val) < 0.0), nil
}

func EvenImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	val := Car(args)
	return BooleanWithValue(IntegerValue(val)%2 == 0), nil
}

func OddImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	val := Car(args)
	return BooleanWithValue(IntegerValue(val)%2 != 0), nil
}

func SignImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	val := Car(args)
	if FloatP(val) {
		return IntegerWithValue(sgn(FloatValue(val))), nil
	} else {
		return IntegerWithValue(intSgn(IntegerValue(val))), nil
	}
}

func LogImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	result = FloatWithValue(math.Log(FloatValue(First(args))))
	return
}

func ExptImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	baseObj := First(args)
	expObj := Second(args)
	if NumberP(baseObj) && NumberP(expObj) {
		floating := FloatP(baseObj) || FloatP(expObj)
		base := FloatValue(baseObj)
		exp := FloatValue(expObj)
		val := math.Pow(base, exp)
		if floating {
			result = FloatWithValue(val)
		} else {
			result = IntegerWithValue(int64(val))
		}
	} else {
		err = ProcessError(fmt.Sprintf("expt expects a numeric arguments, received %s, %s", String(First(args)), String(Second(args))), env)
	}
	return
}

func PowImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	areFloats, err := anyFloats(args, env)
	if err != nil {
		return
	}

	base := Car(args)
	exponent := Cadr(args)

	if areFloats {
		return FloatWithValue(math.Pow(FloatValue(base), FloatValue(exponent))), nil
	} else {
		ret := int64(1)
		b := IntegerValue(base)
		e := IntegerValue(exponent)
		for e > 0 {
			if e&1 != 0 {
				ret *= b
			}
			b *= b
			e >>= 1
		}
		return IntegerWithValue(ret), nil
	}
}

func IsInfImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	val := Car(args)
	if !NumberP(val) {
		err = ProcessError(fmt.Sprintf("inf? expected a nunber, received %s", String(val)), env)
		return
	}

	if FloatP(val) {
		return BooleanWithValue(math.IsInf(float64(FloatValue(val)), 0)), nil
	} else {
		return BooleanWithValue(false), nil
	}
}

func IsNaNImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	val := Car(args)
	if !NumberP(val) {
		err = ProcessError(fmt.Sprintf("nan? expected a nunber, received %s", String(val)), env)
		return
	}

	if FloatP(val) {
		return BooleanWithValue(math.IsNaN(float64(FloatValue(val)))), nil
	} else {
		return BooleanWithValue(false), nil
	}
}

func FloatToBitsImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	float := Car(args)
	return IntegerWithValue(int64(math.Float32bits(float32(FloatValue(float))))), nil
}

func BitsToFloatImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	bits := Car(args)
	return FloatWithValue(float64(math.Float32frombits(uint32(IntegerValue(bits))))), nil
}

func DoubleToBitsImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	double := Car(args)
	return IntegerWithValue(int64(math.Float64bits(FloatValue(double)))), nil
}

func BitsToDoubleImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	bits := Car(args)
	return FloatWithValue(math.Float64frombits(uint64(IntegerValue(bits)))), nil
}
