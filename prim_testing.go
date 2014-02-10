// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the testing primitive functions.

package golisp

func RegisterTestingPrimitives() {
    MakePrimitiveFunction("describe", -1, DescribeImpl)
}