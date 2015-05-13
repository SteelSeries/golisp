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
	MakePrimitiveFunction("load", 1, LoadFileImpl)
	MakePrimitiveFunction("write-line", -1, WriteLineImpl)
	MakePrimitiveFunction("open-output-file", 1, OpenOutputFileImpl)
	MakePrimitiveFunction("close-output-port", 1, CloseOutputPortImpl)
	MakePrimitiveFunction("write-string", 2, WriteStringImpl)
	MakePrimitiveFunction("newline", 1, NewlineImpl)
}

func LoadFileImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	filename, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !StringP(filename) {
		err = ProcessError("Filename must be a string", env)
		return
	}

	return ProcessFile(StringValue(filename))
}

func WriteLineImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	str, err := concatStringForms(args, env)
	if err != nil {
		return
	}
	println(str)
	return
}

func OpenOutputFileImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	filename, err := Eval(Car(args), env)
	if err != nil {
		return
	}

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
	p, err := Eval(Car(args), env)
	if err != nil {
		return
	}

	if !PortP(p) {
		err = ProcessError("close-output-port expects its argument be a port", env)
		return
	}

	(*os.File)(PortValue(p)).Close()
	return

}

func WriteStringImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	str, err := Eval(Car(args), env)
	if err != nil {
		return
	}

	if !StringP(str) {
		err = ProcessError("write-string expects its first argument to be a string", env)
		return
	}

	p, err := Eval(Cadr(args), env)
	if err != nil {
		return
	}

	if !PortP(p) {
		err = ProcessError("write-string expects its second argument be a port", env)
		return
	}

	_, err = (*os.File)(PortValue(p)).WriteString(StringValue(str))

	if err != nil {
		return
	}

	return
}

func NewlineImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	p, err := Eval(Car(args), env)
	if err != nil {
		return
	}

	if !PortP(p) {
		err = ProcessError("newline expects its argument be a port", env)
		return
	}

	_, err = (*os.File)(PortValue(p)).WriteString("\n")

	if err != nil {
		return
	}

	return
}
