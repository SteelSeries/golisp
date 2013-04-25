// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file define the expression interface

package golisp

type Expression interface {
    IntValue() int
    StringValue() string
    BooleanValue() bool
    IdentifierValue() string

    Head() Expression
    Tail() Expression
    Length() Number
    Eval() Expression
    IsEqual(other Expression) Boolean
    IsNil() Boolean
    NotNil() Boolean
    IsList() Boolean

    String() string
}
