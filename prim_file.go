// Copyright 2017 Dave Astels.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the file related primitive functions.

package golisp

import (
	"os"
	"path/filepath"
)

func RegisterFilePrimitives() {
	MakeTypedPrimitiveFunction("file-exists?", "1", fileExistsImpl, []uint32{StringType})
	MakeTypedPrimitiveFunction("file-directory?", "1", directoryReadImpl, []uint32{StringType})
	MakeTypedPrimitiveFunction("file-modification-time", "1", fileModificationTimeImpl, []uint32{StringType})
	MakeTypedPrimitiveFunction("directory-read", "1", listDirectoryImpl, []uint32{StringType})
}

func fileExistsImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	_, ferr := os.Stat(StringValue(Car(args)))
	return BooleanWithValue(!os.IsNotExist(ferr)), nil
}

func directoryReadImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	info, err := os.Stat(StringValue(Car(args)))
	if err != nil {
		if os.IsNotExist(err) {
			return LispFalse, nil
		}
		return
	}
	return BooleanWithValue(info.IsDir()), nil
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

func listDirectoryImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	dir := StringValue(First(args))
	pattern := filepath.Join(dir, "*")

	filenames, err := filepath.Glob(pattern)
	if err != nil {
		return
	}

	names := make([]*Data, len(filenames))
	for index, fname := range filenames {
		fname, _ = filepath.Abs(fname)
		names[index] = StringWithValue(fname)
	}
	return ArrayToList(names), nil
}
