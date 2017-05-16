// Copyright 2017 Dave Astels.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the file related primitive functions.

package golisp

import (
	"os"
)

func RegisterFilePrimitives() {
	MakeTypedPrimitiveFunction("file-exists?", "1", fileExistsImpl, []uint32{StringType})
	MakeTypedPrimitiveFunction("file-modification-time", "1", fileModificationTimeImpl, []uint32{StringType})
}

func fileExistsImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	_, ferr := os.Stat(StringValue(Car(args)))
	result = BooleanWithValue(!os.IsNotExist(ferr))
	return
}

func fileModificationTimeImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	info, err := os.Stat(StringValue(Car(args)))
	if err != nil {
		if os.IsNotExist(err) {
			return LispFalse, nil
		}
		return
	}
	return IntegerWithValue(info.ModTime().Unix()), nil
}
