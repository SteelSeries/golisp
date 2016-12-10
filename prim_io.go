// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the i/o primitive functions.

package golisp

import (
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"unicode"
)

func RegisterIOPrimitives() {
	MakeTypedPrimitiveFunction("open-output-file", "1|2", OpenOutputFileImpl, []uint32{StringType, BooleanType})
	MakeTypedPrimitiveFunction("open-input-file", "1", OpenInputFileImpl, []uint32{StringType})
	MakeTypedPrimitiveFunction("close-port", "1", ClosePortImpl, []uint32{PortType})
	MakeTypedPrimitiveFunction("write-bytes", "2", WriteBytesImpl, []uint32{BoxedObjectType, PortType})
	MakeTypedPrimitiveFunction("write-string", "1|2", WriteStringImpl, []uint32{StringType, PortType})
	MakeTypedPrimitiveFunction("write", "1|2", WriteImpl, []uint32{AnyType, PortType})
	MakeTypedPrimitiveFunction("newline", "0|1", NewlineImpl, []uint32{PortType})
	MakeTypedPrimitiveFunction("read-string", "0|1", ReadStringImpl, []uint32{PortType})
	MakeTypedPrimitiveFunction("read", "1", ReadImpl, []uint32{PortType})
	MakePrimitiveFunction("eof-object?", "1", EofObjectImpl)
	MakeTypedPrimitiveFunction("list-directory", "1|2", ListDirectoryImpl, []uint32{StringType, StringType})
	MakeTypedPrimitiveFunction("format", ">=2", FormatImpl, []uint32{BooleanType | PortType, StringType, AnyType})
}

func OpenOutputFileImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	filename := First(args)
	var openFlag = os.O_WRONLY | os.O_CREATE | os.O_TRUNC
	if Length(args) == 2 && BooleanValue(Second(args)) {
		openFlag = os.O_WRONLY | os.O_CREATE | os.O_APPEND
	}

	f, err := os.OpenFile(StringValue(filename), openFlag, 0666)
	if err == nil {
		result = PortWithValue(f)
	}
	return
}

func OpenInputFileImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	filename := First(args)
	f, err := os.Open(StringValue(filename))
	if err == nil {
		result = PortWithValue(f)
	}
	return
}

func ClosePortImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	p := First(args)
	if !PortP(p) {
		err = ProcessError("close-port expects its argument be a port", env)
	} else {
		(*os.File)(PortValue(p)).Close()
	}
	return
}

func WriteBytesImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	bytes := First(args)
	if ObjectType(bytes) != "[]byte" {
		err = ProcessError("write expects its first argument to be a bytearray", env)
		return
	}

	p := Second(args)
	_, err = (*os.File)(PortValue(p)).Write(*(*[]byte)(ObjectValue(bytes)))
	return
}

func WriteStringImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	str := First(args)
	var port *os.File
	if Length(args) == 1 {
		port = os.Stdout
	} else {
		port = PortValue(Second(args))
	}

	_, err = port.WriteString(StringValue(str))
	return
}

func WriteImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var port *os.File

	if Length(args) == 1 {
		port = os.Stdout
	} else {
		p := Second(args)
		if !PortP(p) {
			err = ProcessError("write expects its second argument be a port", env)
			return
		}
		port = PortValue(p)
	}

	_, err = port.WriteString(String(First(args)))
	return
}

func NewlineImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var port *os.File

	if Length(args) == 0 {
		port = os.Stdout
	} else {
		port = PortValue(First(args))
	}

	_, err = port.WriteString("\n")
	return
}

func ReadStringImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var port *os.File
	if Length(args) == 0 {
		port = os.Stdin
	} else {
		port = PortValue(First(args))
	}

	// if Length(args) == 2 {
	// 	charset := Second(args)
	// 	if !StringP(charset) {
	// 		err = ProcessError(fmt.Sprintf("read-string expects its optional second argument to be a string, but got %s", String(charset)), env)
	// 		return
	// 	}
	// }

	info, err := port.Stat()
	if err != nil {
		return
	}

	readBuffer := make([]byte, info.Size())

	n, err := port.Read(readBuffer)
	if err == nil {
		result = StringWithValue(string(readBuffer[:n]))
	}
	return
}

func ReadImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var port *os.File

	if Length(args) == 0 {
		port = os.Stdin
	} else {
		port = PortValue(First(args))
	}

	result, err = ParseObjectFromFileInEnv(port, env)
	return
}

func EofObjectImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(IsEqual(First(args), EofObject)), nil
}

func ListDirectoryImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	dir := StringValue(First(args))
	fpart := "*"
	if Length(args) == 2 {
		fpart = StringValue(Second(args))
	}
	pattern := filepath.Join(dir, fpart)

	filenames, err := filepath.Glob(pattern)
	if err != nil {
		return
	}

	names := make([]*Data, 0, 0)
	for _, fname := range filenames {
		names = append(names, StringWithValue(fname))
	}
	return ArrayToList(names), nil
}

func FormatImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	destination := First(args)
	if !BooleanP(destination) && !PortP(destination) {
		err = ProcessError(fmt.Sprintf("format expects its second argument be a boolean or port, but was %s", String(destination)), env)
		return
	}

	controlStringObj := Second(args)
	controlString := StringValue(controlStringObj)

	arguments := Cddr(args)

	numberOfSubstitutions := strings.Count(controlString, "~")
	if numberOfSubstitutions != Length(arguments) {
		err = ProcessError("number of replacements in the control string and number of arguments must be equal", env)
		return
	}

	parts := make([]string, 0, numberOfSubstitutions*2+1)
	start := 0
	var i int
	var numericArg int
	var atModifier bool
	var substitution string
	var padding string
	var n int64

	for i < len(controlString) {
		if controlString[i] == '~' { // start of a substitution
			parts = append(parts, controlString[start:i])
			i++
			start = i
			for unicode.IsDigit(rune(controlString[i])) {
				i++
			}
			if i == start {
				if controlString[i] == '#' {
					numericArg = Length(arguments)
					i++
				} else if controlString[i] == 'V' || controlString[i] == 'v' {
					if IntegerP(Car(arguments)) {
						numericArg = int(IntegerValue(Car(arguments)))
						arguments = Cdr(arguments)
					} else {
						err = ProcessError(fmt.Sprintf("format encountered a size argument mismatch at index %d", i), env)
						return
					}
					i++
				} else {
					numericArg = 0
				}
			} else {
				n, err = strconv.ParseInt(string(controlString[start:i]), 10, 64)
				if err != nil {
					return
				}
				numericArg = int(n)
			}
			if controlString[i] == '@' {
				atModifier = true
				i++
			}
			switch controlString[i] {
			case 'A', 'a':
				substitution = PrintString(Car(arguments))
				if len(substitution) < numericArg {
					padding = strings.Repeat(" ", numericArg-len(substitution))
				} else {
					padding = ""
				}
				if atModifier {
					parts = append(parts, padding)
				}
				parts = append(parts, substitution)
				if !atModifier {
					parts = append(parts, padding)
				}
				arguments = Cdr(arguments)
				start = i + 1

			case 'S', 's':
				substitution = String(Car(arguments))
				if len(substitution) < numericArg {
					padding = strings.Repeat(" ", numericArg-len(substitution))
				} else {
					padding = ""
				}
				if atModifier {
					parts = append(parts, padding)
				}
				parts = append(parts, substitution)
				if !atModifier {
					parts = append(parts, padding)
				}
				arguments = Cdr(arguments)
				start = i + 1

			case '%':
				if numericArg > 0 {
					parts = append(parts, strings.Repeat("\n", numericArg))
				} else {
					parts = append(parts, "\n")
				}
				start = i + 1

			case '~':
				if numericArg > 0 {
					parts = append(parts, strings.Repeat("~", numericArg))
				} else {
					parts = append(parts, "~")
				}
				start = i + 1

			case '\n':
				for i < len(controlString) && unicode.IsSpace(rune(controlString[i])) {
					i++
				}
				if atModifier {
					parts = append(parts, "\n")
				}
				start = i
				i--

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

	combinedString := strings.Join(parts, "")

	if PortP(destination) {
		port := PortValue(destination)
		_, err = port.WriteString(combinedString)
	} else if BooleanValue(destination) {
		_, err = os.Stdout.WriteString(combinedString)
	} else {
		result = StringWithValue(combinedString)
	}

	return
}
