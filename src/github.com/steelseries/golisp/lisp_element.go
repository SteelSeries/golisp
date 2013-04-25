// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file inmpliments the LispElement interface
package golisp

type LispElement interface {
    Eval() LispElement
    String() string
    NumericValue() int
    BooleanValue() bool
}
