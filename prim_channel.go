// Copyright 2015 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the go channel primitive functions.

package golisp

import (
	"fmt"
	"unsafe"
)

type Channel chan *Data

func RegisterChannelPrimitives() {
	MakePrimitiveFunction("make-channel", "0|1", MakeChannelImpl)
	MakePrimitiveFunction("channel<-", "2", ChannelWriteImpl)
	MakePrimitiveFunction("<-channel", "1", ChannelReadImpl)
	MakePrimitiveFunction("channel-try-write", "2", ChannelTryWriteImpl)
	MakePrimitiveFunction("channel-try-read", "1", ChannelTryReadImpl)
	MakePrimitiveFunction("close-channel", "1", CloseChannelImpl)
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
		c = make(Channel)
	}

	return ObjectWithTypeAndValue("Channel", unsafe.Pointer(&c)), nil
}

func ChannelWriteImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	channelObj := Car(args)
	if !ObjectP(channelObj) || ObjectType(channelObj) != "Channel" {
		err = ProcessError(fmt.Sprintf("channel<- expects an Channel object but received %s.", ObjectType(channelObj)), env)
		return
	}

	c := *(*Channel)(ObjectValue(channelObj))
	obj := Cadr(args)

	func() {
		defer func() {
			if e := recover(); e != nil {
				err = ProcessError("channel<- tried to write to a closed channel.", env)
			}
		}()
		c <- obj
	}()

	if err != nil {
		return
	}

	return
}

func ChannelReadImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	channelObj := Car(args)
	if !ObjectP(channelObj) || ObjectType(channelObj) != "Channel" {
		err = ProcessError(fmt.Sprintf("<-channel expects an Channel object but received %s.", ObjectType(channelObj)), env)
		return
	}

	c := *(*Channel)(ObjectValue(channelObj))

	obj, more := <-c

	return ArrayToList([]*Data{obj, BooleanWithValue(more)}), nil
}

func ChannelTryWriteImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	channelObj := Car(args)
	if !ObjectP(channelObj) || ObjectType(channelObj) != "Channel" {
		err = ProcessError(fmt.Sprintf("channel-try-write expects an Channel object but received %s.", ObjectType(channelObj)), env)
		return
	}

	c := *(*Channel)(ObjectValue(channelObj))
	obj := Cadr(args)
	writeSucceeded := true

	func() {
		defer func() {
			if e := recover(); e != nil {
				err = ProcessError("channel-try-write tried to write to a closed channel.", env)
			}
		}()
		select {
		case c <- obj:
		default:
			writeSucceeded = false
		}
	}()

	if err != nil {
		return
	}

	return BooleanWithValue(writeSucceeded), nil
}

func ChannelTryReadImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	channelObj := Car(args)
	if !ObjectP(channelObj) || ObjectType(channelObj) != "Channel" {
		err = ProcessError(fmt.Sprintf("<-channel expects an Channel object but received %s.", ObjectType(channelObj)), env)
		return
	}

	c := *(*Channel)(ObjectValue(channelObj))

	var obj *Data
	more := true
	readSucceed := true

	select {
	case obj, more = <-c:
	default:
		readSucceed = false
	}

	return ArrayToList([]*Data{BooleanWithValue(readSucceed), obj, BooleanWithValue(more)}), nil
}

func CloseChannelImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	channelObj := Car(args)
	if !ObjectP(channelObj) || ObjectType(channelObj) != "Channel" {
		err = ProcessError(fmt.Sprintf("<-channel expects an Channel object but received %s.", ObjectType(channelObj)), env)
		return
	}

	c := *(*Channel)(ObjectValue(channelObj))

	func() {
		defer func() {
			if e := recover(); e != nil {
				err = ProcessError("channel-close tried to close a channel twice.", env)
			}
		}()
		close(c)
	}()

	return
}
