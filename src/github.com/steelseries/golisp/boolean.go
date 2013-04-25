// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file inmpliments the number atom
package golisp

type Boolean struct {
    value bool
}

var False = Boolean{false}
var True = Boolean{true}

func (self Boolean) IntValue() int {
    return 0
}

func (self Boolean) StringValue() string {
    return ""
}

func (self Boolean) BooleanValue() bool {
    return self.value
}

func (self Boolean) IdentifierValue() string {
    return ""
}

func (self Boolean) Head() Expression {
    return nil
}

func (self Boolean) Tail() Expression {
    return nil
}

func (self Boolean) Length() Number {
    return NumberWithValue(0)
}

func (self Boolean) Eval() Expression {
    return self
}

func (self Boolean) IsEqual(other Expression) Boolean {
    if self.value && other.BooleanValue() {
        return True
    } else {
        return False
    }
}

func (self Boolean) IsNil() Boolean {
    return False
}

func (self Boolean) NotNil() Boolean {
    return True
}

func (self Boolean) IsList() Boolean {
    return False
}

func (self Boolean) String() string {
    if self.value {
        return "#t"
    } else {
        return "#f"
    }
}
