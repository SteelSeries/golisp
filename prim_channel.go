// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the concurrency primitive functions.

package golisp

import (
	"fmt"
	"unsafe"
)

type Channel chan *Data

func RegisterChannelPrimitives() {
	MakePrimitiveFunction("make-channel", "0|1", MakeChannelImpl)
	MakePrimitiveFunction("->", "2", WriteToChannelImpl)
	MakePrimitiveFunction("<-", "1", ReadFromChannelImpl)
}

func MakeChannelImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var c Channel

	if Length(args) == 1 {
		lengthObj := Car(args)
		if !IntegerP(lengthObj) {
			err = ProcessError(fmt.Sprintf("make-channel expects an Integer as its second argument but received %s.", TypeName(TypeOf(lengthObj))), env)
			return
		}

		channelLength := IntegerValue(lengthObj)

		if channelLength < 0 {
			err = ProcessError(fmt.Sprintf("channel size needs to be positive; got %d.", channelLength), env)
			return
		}

		const maxInt = int64(int(^uint(0) >> 1))

		if channelLength > maxInt {
			err = ProcessError(fmt.Sprintf("channel size is too big; got %d, max is %d.", channelLength, maxInt), env)
			return
		}

		c = make(Channel, int(channelLength))
	} else {
		fmt.Printf("TEST\n")
		c = make(Channel)
	}

	return ObjectWithTypeAndValue("Channel", unsafe.Pointer(&c)), nil
}

func WriteToChannelImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	channelObj := Car(args)
	if !ObjectP(channelObj) || ObjectType(channelObj) != "Channel" {
		err = ProcessError(fmt.Sprintf("-> expects an Channel object but received %s.", ObjectType(channelObj)), env)
		return
	}

	c := *(*Channel)(ObjectValue(channelObj))

	writeObj := Cadr(args)

	c <- writeObj

	return
}

func ReadFromChannelImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	channelObj := Car(args)
	if !ObjectP(channelObj) || ObjectType(channelObj) != "Channel" {
		err = ProcessError(fmt.Sprintf("<- expects an Channel object but received %s.", ObjectType(channelObj)), env)
		return
	}

	c := *(*Channel)(ObjectValue(channelObj))

	return <-c, nil
}
