// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file inmpliments the number atom
package golisp

type NilType struct {
}

var Nil = NilType{}

func (self NilType) IntValue() int {
    return 0
}

func (self NilType) StringValue() string {
    return ""
}

func (self NilType) BooleanValue() bool {
    return false
}

func (self NilType) IdentifierValue() string {
    return ""
}

func (self NilType) Head() Expression {
    return Nil
}

func (self NilType) Tail() Expression {
    return Nil
}

func (self NilType) Length() Number {
    return NumberWithValue(0)
}

func (self NilType) Eval() Expression {
    return Nil
}

func (self NilType) IsEqual(other Expression) Boolean {
    _, ok := other.(NilType)
    if ok {
        return True
    } else {
        return False
    }
}

func (self NilType) IsNil() Boolean {
    return False
}

func (self NilType) NotNil() Boolean {
    return True
}

func (self NilType) IsList() Boolean {
    return False
}

func (self NilType) String() string {
    return "()"
}
