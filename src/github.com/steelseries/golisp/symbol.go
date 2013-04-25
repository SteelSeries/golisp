// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file impliments the string atom

package golisp

type Symbol struct {
    name string
}

func SymbolWithName(s string) Symbol {
    return Symbol{s}
}

func (self Symbol) IntValue() int {
    return 0
}

func (self Symbol) StringValue() string {
    return self.name
}

func (self Symbol) BooleanValue() bool {
    return true
}

func (self Symbol) IdentifierValue() string {
    return self.name
}

func (self Symbol) Head() Expression {
    return nil
}

func (self Symbol) Tail() Expression {
    return nil
}

func (self Symbol) Length() Number {
    return Number{0}
}

func (self Symbol) Eval() Expression {
    return nil
}

func (self Symbol) IsEqual(other Expression) Boolean {
    if self.name == other.IdentifierValue() {
        return True
    } else {
        return False
    }
}

func (self Symbol) IsNil() Boolean {
    return False
}

func (self Symbol) NotNil() Boolean {
    return True
}

func (self Symbol) IsList() Boolean {
    return False
}

func (self Symbol) String() string {
    return self.name
}
