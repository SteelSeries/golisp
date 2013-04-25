// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file inmpliments the number atom
package golisp

import (
    "fmt"
)

type Number struct {
    value int
}

func NumberWithValue(i int) Number {
    return Number{i}
}

func (self Number) IntValue() int {
    return self.value
}

func (self Number) StringValue() string {
    return ""
}

func (self Number) BooleanValue() bool {
    return true
}

func (self Number) IdentifierValue() string {
    return ""
}

func (self Number) Head() Expression {
    return nil
}

func (self Number) Tail() Expression {
    return nil
}

func (self Number) Length() Number {
    return NumberWithValue(0)
}

func (self Number) Eval() Expression {
    return self
}

func (self Number) IsEqual(other Expression) Boolean {
    if self.value == other.IntValue() {
        return True
    } else {
        return False
    }
}

func (self Number) IsNil() Boolean {
    return False
}

func (self Number) NotNil() Boolean {
    return True
}

func (self Number) IsList() Boolean {
    return False
}

func (self Number) String() string {
    return fmt.Sprintf("%d", self.value)
}
