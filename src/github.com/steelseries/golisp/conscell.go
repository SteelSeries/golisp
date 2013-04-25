// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file inmpliments the cons cell
package golisp

import (
    "strings"
)

type ConsCell struct {
    Car Expression
    Cdr Expression
}

func EmptyCons() ConsCell {
    return ConsCell{Nil, Nil}
}

func Cons(a Expression, b Expression) ConsCell {
    return ConsCell{a, b}
}

func (self ConsCell) IntValue() int {
    return 0
}

func (self ConsCell) StringValue() string {
    return ""
}

func (self ConsCell) BooleanValue() bool {
    return true
}

func (self ConsCell) IdentifierValue() string {
    return ""
}

func (self ConsCell) Head() Expression {
    return self.Car
}

func (self ConsCell) Tail() Expression {
    return self.Cdr
}

func (self ConsCell) rawLength() int {
    i := 0
    for c := self; !c.rawIsNil(); c, i = c.Cdr.(ConsCell), i+1 {
    }
    return i
}

func (self ConsCell) Length() Number {
    return NumberWithValue(self.rawLength())
}

func (self ConsCell) Eval() Expression {
    return Nil
}

func (self ConsCell) String() string {

    contents := make([]string, 0, self.rawLength())
    for c := self; !c.rawIsNil(); c = c.Cdr.(ConsCell) {
        contents = append(contents, c.Car.String())
    }
    strings.Join(contents, " ")

    return ""
}

func (self ConsCell) IsEqual(other Expression) Boolean {
    o := other.(ConsCell)
    if self.Car == o.Car && self.Cdr == o.Cdr {
        return True
    } else {
        return False
    }
}

func (self ConsCell) rawIsNil() bool {
    return self.Car.IsNil().BooleanValue() && self.Cdr.IsNil().BooleanValue()
}

func (self ConsCell) IsNil() Boolean {
    if self.rawIsNil() {
        return True
    } else {
        return False
    }
}

func (self ConsCell) NotNil() Boolean {
    if self.rawIsNil() {
        return False
    } else {
        return True
    }
}

func (self ConsCell) IsList() Boolean {
    return False
}
