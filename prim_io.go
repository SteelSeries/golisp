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
	MakePrimitiveFunction("open-input-file", "1", OpenInputFileImpl)
	MakePrimitiveFunction("open-output-file", "1|2", OpenOutputFileImpl)
	MakePrimitiveFunction("close-port", "1", ClosePortImpl)
	MakePrimitiveFunction("write-bytes", "2", WriteBytesImpl)

	MakePrimitiveFunction("write-string", "1|2", WriteStringImpl)
	MakePrimitiveFunction("newline", "0|1", NewlineImpl)
	MakePrimitiveFunction("write", "1|2", WriteImpl)
	MakePrimitiveFunction("read-string", "0|1", ReadStringImpl)
	MakePrimitiveFunction("read", "1", ReadImpl)
	MakePrimitiveFunction("eof-object?", "1", EofObjectImpl)

	MakePrimitiveFunction("list-directory", "1|2", ListDirectoryImpl)

	MakePrimitiveFunction("format", ">=2", FormatImpl)
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

func ReadStringImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var port *os.File
	p := First(args)
	if Length(args) == 0 {
		port = os.Stdin
	} else {
		if !PortP(p) {
			err = ProcessError(fmt.Sprintf("read-string expects its first argument be a port, but got %s", String(p)), env)
			return
		}
		port = PortValue(p)
	}

	// if Length(args) == 2 {
	// 	charset := Second(args)
	// 	if !StringP(charset) {
	// 		err = ProcessError(fmt.Sprintf("read-string expects its optional second argument to be a string, but got %s", String(charset)), env)
	// 		return
	// 	}
	// }

	readBuffer := make([]byte, 1024)

	n, err := port.Read(readBuffer)
	if n < 1024 {
		result = StringWithValue(string(readBuffer[:n]))
		err = nil
		return
	} else if err != nil {
		return
	}
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

func ListDirectoryImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	dir := StringValue(Car(args))
	fpart := "*"
	if Length(args) == 2 {
		fpart = StringValue(Cadr(args))
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
	destination := Car(args)
	if !BooleanP(destination) && !PortP(destination) {
		err = ProcessError(fmt.Sprintf("format expects its second argument be a boolean or port, but was %s", String(destination)), env)
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

	if i < len(controlString) || !NilP(arguments) {
		err = ProcessError("number of replacements in the control string and number of arguments must be equal", env)
		return
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
