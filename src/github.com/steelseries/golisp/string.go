// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

//This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file inmpliments the string atom
package golisp

import "fmt"

type String struct {
    value string
}

func StringWithValue(s string) String {
    return String{s}
}

func (self String) IntValue() int {
    return 0
}

func (self String) StringValue() string {
    return self.value
}

func (self String) BooleanValue() bool {
    return true
}

func (self String) IdentifierValue() string {
    return ""
}

func (self String) Head() Expression {
    return nil
}

func (self String) Tail() Expression {
    return nil
}

func (self String) Length() Number {
    return NumberWithValue(0)
}

func (self String) Eval() Expression {
    return self
}

func (self String) IsEqual(other Expression) Boolean {
    if self.value == other.StringValue() {
        return True
    } else {
        return False
    }
}

func (self String) IsNil() Boolean {
    return False
}

func (self String) NotNil() Boolean {
    return True
}

func (self String) IsList() Boolean {
    return False
}

func (self String) String() string {
    return fmt.Sprintf("%s", self.value)
}
