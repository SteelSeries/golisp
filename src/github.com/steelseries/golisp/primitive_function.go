// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file inmpliments primitive functions.
package golisp

import (
    "fmt"
)

type PrimitiveFunction struct {
}

func (self *PrimitiveFunction) String() string {
    return fmt.Sprintf("<prim: ?>")
}
