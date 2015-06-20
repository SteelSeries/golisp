// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the i/o primitive functions.

package golisp

import (
	"os"
)

func RegisterIOPrimitives() {
	MakeRestrictedPrimitiveFunction("open-output-file", "1", OpenOutputFileImpl)
	MakeRestrictedPrimitiveFunction("close-output-port", "1", CloseOutputPortImpl)
	MakeRestrictedPrimitiveFunction("write-string", "2", WriteStringImpl)
	MakeRestrictedPrimitiveFunction("write-bytes", "2", WriteBytesImpl)
}

func OpenOutputFileImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	filename := Car(args)
	if !StringP(filename) {
		err = ProcessError("open-output-port expects its argument to be a string", env)
		return
	}

	f, err := os.Create(StringValue(filename))
	if err != nil {
		return
	}
	return PortWithValue(f), nil
}

func CloseOutputPortImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	p := Car(args)
	if !PortP(p) {
		err = ProcessError("close-output-port expects its argument be a port", env)
		return
	}

	(*os.File)(PortValue(p)).Close()
	return

}

func WriteStringImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	str := Car(args)
	if !StringP(str) {
		err = ProcessError("write-string expects its first argument to be a string", env)
		return
	}

	p := Cadr(args)
	if !PortP(p) {
		err = ProcessError("write-string expects its second argument be a port", env)
		return
	}

	_, err = (*os.File)(PortValue(p)).WriteString(StringValue(str))

	return
}

func WriteBytesImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	bytes := Car(args)
	if !ObjectP(bytes) || ObjectType(bytes) != "[]byte" {
		err = ProcessError("write expects its first argument to be a bytearray", env)
		return
	}

	p := Cadr(args)
	if !PortP(p) {
		err = ProcessError("write expects its second argument be a port", env)
		return
	}

	_, err = (*os.File)(PortValue(p)).Write(*(*[]byte)(ObjectValue(bytes)))

	return
}
