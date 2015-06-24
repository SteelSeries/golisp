// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the i/o primitive functions.

package golisp

import (
	"fmt"
	"os"
	"strings"
)

func RegisterIOPrimitives() {
	MakeRestrictedPrimitiveFunction("open-input-file", "1", OpenInputFileImpl)
	MakeRestrictedPrimitiveFunction("open-output-file", "1|2", OpenOutputFileImpl)
	MakeRestrictedPrimitiveFunction("close-port", "1", ClosePortImpl)
	MakeRestrictedPrimitiveFunction("write-bytes", "2", WriteBytesImpl)

	MakePrimitiveFunction("write-string", "1|2", WriteStringImpl)
	MakePrimitiveFunction("newline", "0|1", NewlineImpl)
	MakePrimitiveFunction("write", "1|2", WriteImpl)
	MakePrimitiveFunction("read", "1", ReadImpl)
	MakePrimitiveFunction("eof-object?", "1", EofObjectImpl)

	MakePrimitiveFunction("format", ">=3", FormatImpl)
}

func OpenOutputFileImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	filename := Car(args)
	if !StringP(filename) {
		err = ProcessError("open-output-port expects its argument to be a string", env)
		return
	}

	var openFlag = os.O_WRONLY | os.O_CREATE | os.O_TRUNC
	if Length(args) == 2 && BooleanValue(Cadr(args)) {
		openFlag = os.O_WRONLY | os.O_CREATE | os.O_APPEND
	}

	f, err := os.OpenFile(StringValue(filename), openFlag, 0666)
	if err != nil {
		return
	}
	return PortWithValue(f), nil
}

func OpenInputFileImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	filename := Car(args)
	if !StringP(filename) {
		err = ProcessError("open-input-port expects its argument to be a string", env)
		return
	}

	f, err := os.Open(StringValue(filename))
	if err != nil {
		return
	}
	return PortWithValue(f), nil
}

func ClosePortImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	p := Car(args)
	if !PortP(p) {
		err = ProcessError("close-port expects its argument be a port", env)
		return
	}

	(*os.File)(PortValue(p)).Close()
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

func WriteStringImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	str := Car(args)
	if !StringP(str) {
		err = ProcessError("write-string expects its first argument to be a string", env)
		return
	}

	var port *os.File
	if Length(args) == 1 {
		port = os.Stdout
	} else {
		p := Cadr(args)
		if !PortP(p) {
			err = ProcessError("write-string expects its second argument be a port", env)
			return
		}
		port = PortValue(p)
	}

	_, err = port.WriteString(StringValue(str))
	return
}

func WriteImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var port *os.File

	if Length(args) == 1 {
		port = os.Stdout
	} else {
		p := Cadr(args)
		if !PortP(p) {
			err = ProcessError("write expects its second argument be a port", env)
			return
		}
		port = PortValue(p)
	}

	_, err = port.WriteString(String(Car(args)))
	return
}

func NewlineImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var port *os.File

	if Length(args) == 0 {
		port = os.Stdout
	} else {
		p := Car(args)
		if !PortP(p) {
			err = ProcessError("newline expects its argument be a port", env)
			return
		}
		port = PortValue(p)
	}

	_, err = port.WriteString("\n")
	return
}

func ReadImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var port *os.File

	if Length(args) == 0 {
		port = os.Stdin
	} else {
		p := Car(args)
		if !PortP(p) {
			err = ProcessError("read expects its argument be a port", env)
			return
		}
		port = PortValue(p)
	}

	result, err = ParseObjectFromFileInEnv(port, env)
	return
}

func EofObjectImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(IsEqual(Car(args), EofObject)), nil
}

func FormatImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	destination := Car(args)
	if !BooleanP(destination) && !PortP(destination) {
		err = ProcessError(fmt.Sprintf("format expects its second argument be a boolean or port, but was %s", String(destination)), env)
		return
	}

	if BooleanP(destination) && BooleanValue(destination) {
		err = ProcessError("format does not support \"current output port\" as a destination", env)
		return
	}

	controlStringObj := Cadr(args)
	if !StringP(controlStringObj) {
		err = ProcessError("format expects its second argument be a string", env)
		return
	}
	controlString := StringValue(controlStringObj)

	arguments := Cddr(args)

	numberOfSubstitutions := strings.Count(controlString, "~")
	parts := make([]string, 0, numberOfSubstitutions*2+1)
	start := 0
	var i int

	for i < len(controlString) {
		if controlString[i] == '~' { // start of a substitution
			parts = append(parts, controlString[start:i])
			i++
			switch controlString[i] {
			case 'A':
				parts = append(parts, PrintString(Car(arguments)))
				arguments = Cdr(arguments)
				start = i + 1

			case 'S':
				parts = append(parts, String(Car(arguments)))
				arguments = Cdr(arguments)
				start = i + 1

			case '%':
				parts = append(parts, "\n")
				start = i + 1

			case '~':
				parts = append(parts, "~")
				start = i + 1

			case '\n':
				start = i + 1

			default:
				err = ProcessError(fmt.Sprintf("format encountered an unsupported substitution at index %d", i), env)
				return
			}
		}
		i++
	}

	if start < len(controlString) {
		parts = append(parts, controlString[start:i])
	}

	if i < len(controlString) || !NilP(arguments) {
		err = ProcessError("number of replacements in the control string and number of arguments must be equal", env)
		return
	}

	combinedString := strings.Join(parts, "")

	if PortP(destination) {
		port := PortValue(destination)
		_, err = port.WriteString(combinedString)
	} else {
		result = StringWithValue(combinedString)
	}

	return
}
