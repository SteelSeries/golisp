// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains Raspberry Pi related primitive functions.

package golisp

import (
	"github.com/mrmorphic/hwio"
)

func RegisterPiPrimitives() {
	Global.BindTo(Intern("gpio:INPUT"), IntegerWithValue(hwio.INPUT))
	Global.BindTo(Intern("gpio:OUTPUT"), IntegerWithValue(hwio.OUTPUT))
	Global.BindTo(Intern("gpio:INPUT_PULLUP"), IntegerWithValue(hwio.INPUT_PULLUP))
	Global.BindTo(Intern("gpio:INPUT_PULLDOWN"), IntegerWithValue(hwio.INPUT_PULLDOWN))
	Global.BindTo(Intern("gpio:HIGH"), IntegerWithValue(hwio.HIGH))
	Global.BindTo(Intern("gpio:LOW"), IntegerWithValue(hwio.LOW))

	MakePrimitiveFunction("gpio:get-pin", "1|2", GPIOGetPinImpl)
	MakePrimitiveFunction("gpio:set-pin-mode", "2", GPIOSetPinModeImpl)
	MakePrimitiveFunction("gpio:pin?", "2", GPIOPinPImpl)
	MakePrimitiveFunction("gpio:digital-write", "2", GPIODigitalWriteImpl)
	MakePrimitiveFunction("gpio:digital-read", "1", GPIODigitalReadImpl)
}

func GPIOGetPinImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if !StringP(First(args)) {
		err = ProcessError(fmt.Sprintf("gpio:get-pin expected a pin name as its first argument but received %s.", String(First(args))))
		return
	}
	pinName := StringValue(First(args))

	pin := 0

	if Length(args) == 1 {
		pin, err = hwio.GetPin(pinName)
		if err != nil {
			return
		}
	} else {
		modeObj = Second(args)
		if !IntegerP(modeObj) {
			err = ProcessError(fmt.Sprintf("gpio:get-pin expects an integer as it's second argument but received %s.", String(modeObj)))
			return
		}
		mode = IntegerValue(modeObj)
		if mode < hwio.INPUT || mode > hwio.INPUT_PULLDOWN {
			err = ProcessError(fmt.Sprintf("gpio:get-pin expected a valid pin mode as its second argument but received %d.", mode))
		}
		pin, err = hwio.GetPinWithMode(pinName, mode)
		if err != nil {
			return
		}
	}

	result = IntegerWithValue(pin)
	return
}

func GPIOSetPinModeImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	pinObj := First(args)
	if !IntegerP(pinObj) {
		err = ProcessError(fmt.Sprintf("gpio:set-pin-mode expected a pin number as its first argument but received %s.", String(pinObj)))
		return
	}
	pin := IntegerValue(pinObj)

	modeObj = Second(args)
	if !IntegerP(modeObj) {
		err = ProcessError(fmt.Sprintf("gpio:set-pin-mode expects an integer as it's second argument but received %s.", String(modeObj)))
		return
	}
	mode = IntegerValue(modeObj)
	if mode < hwio.INPUT || mode > hwio.INPUT_PULLDOWN {
		err = ProcessError(fmt.Sprintf("gpio:set-pin-mode expected a valid pin mode as its second argument but received %d.", mode))
	}

	hwio.PinMode(pin, mode)
	result = LispTrue
	return
}

func GPIODigitalWriteImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	pinObj := First(args)
	if !IntegerP(pinObj) {
		err = ProcessError(fmt.Sprintf("gpio:digital-write expected a pin number as its first argument but received %s.", String(pinObj)))
		return
	}
	pin := IntegerValue(pinObj)

	valueObj = Second(args)
	value := 0
	if IntegerP(valueObj) {
		value = IntegerValue(valueObj)
		if value != hwio.HIGH && value != hwio.LOW {
			err = ProcessError(fmt.Sprintf("gpio:digital-write expected a valid value as its second argument but received %d.", value))
			return
		}
	} else if BooleanP(valueObj) {
		if BooleanValue(valueObj) {
			value = 1
		} else {
			value = 0
		}
	} else {
		err = ProcessError(fmt.Sprintf("gpio:digital-write expected %d, %d, #f, or #t as its second argument but received %s.", hwio.Low, hwio.HIGH, String(valueObj)))
		return
	}

	hwio.DigitalWrite(pin, int(value))
	result = LispTrue
	return
}

func GPIODigitalReadImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	pinObj := First(args)
	if !IntegerP(pinObj) {
		err = ProcessError(fmt.Sprintf("gpio:digital-read expected a pin number as its first argument but received %s.", String(pinObj)))
		return
	}
	pin := IntegerValue(pinObj)

	value, err := hwio.DigitalRead(pin)
	if err != nil {
		return
	}

	result = BooleanWithValue(value == hwio.HIGH)
	return
}
