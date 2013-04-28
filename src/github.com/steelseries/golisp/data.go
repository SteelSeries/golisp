// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file inmpliments the cons cell
package golisp

import (
    "fmt"
    "strings"
)

const (
    ConsCellType = iota
    NumberType
    BooleanType
    StringType
    SymbolType
    FunctionType
    PrimativeType
)

type Data struct {
    Type         int
    Car          *Data
    Cdr          *Data
    String       string
    Number       int
    FuncionValue *Data
    Primative    *PrimativeFunction
}

// Creation functions

func Cons(car *Data, cdr *Data) *Data {
    return &Data{Type: ConsCellType, Car: car, Cdr: cdr, String: "", Number: 0}
}

func NumberWithValue(n int) *Data {
    return &Data{Type: NumberType, Car: nil, Cdr: nil, String: "", Number: n}
}

func BooleanWithValue(b bool) *Data {
    var num = 0
    if b {
        num = 1
    }
    return &Data{Type: BooleanType, Car: nil, Cdr: nil, String: "", Number: num}
}

func StringWithValue(s string) *Data {
    return &Data{Type: StringType, Car: nil, Cdr: nil, String: s, Number: 0}
}

func SymbolWithValue(s string) *Data {
    return &Data{Type: SymbolType, Car: nil, Cdr: nil, String: s, Number: 0}
}

func IntValue(d *Data) int {
    if d == nil {
        return 0
    }

    if d.Type == NumberType {
        return d.Number
    }

    return 0
}

func StringValue(d *Data) string {
    if d == nil {
        return ""
    }

    if d.Type == StringType || d.Type == SymbolType {
        return d.String
    }

    return ""
}

func BooleanValue(d *Data) bool {
    if d == nil {
        return false
    }

    if d.Type == BooleanType {
        if d.Number == 0 {
            return false
        } else {
            return true
        }
    }

    return false
}

func Car(d *Data) *Data {
    if d == nil {
        return nil
    }

    if d.Type == ConsCellType {
        return d.Car
    }

    return nil
}

func Cdr(d *Data) *Data {
    if d == nil {
        return nil
    }

    if d.Type == ConsCellType {
        return d.Cdr
    }

    return nil
}

func Length(d *Data) {
    if d == nil {
        return 0
    }

    if d.Type == ConsCellType {
        return 1 + Length(d.Cdr)
    }

    return 0
}

func IsEqual(d *Data, o *Data) {
    return d == o || *d == *o
}

func IsNil(d *Data) {
    return d == nil
}

func NotNil(d *Data) {
    return d != nil
}

func IsList(d *Data) {
    return d.Type == ConsCellType
}

func String(d *Data) {
    if d == nil {
        return "()"
    }

    switch d.Type {
    case ConsCellType:
        {
            contents := make([]string, 0, Length(d))
            c = d
            for c := d; NotNil(Cdr(c)); c = Cdr(c) {
                contents = append(contents, (String(Car(c))))
            }
            return fmt.Sprintf("(%s)", strings.Join(contents, " "))
        }
    case NumberType:
        return fmt.Sprintf("%d", d.Number)
    case BooleanType:
        if d.Number == 0 {
            return "#f"
        } else {
            return "#t"
        }
    case StringType:
        return d.String
    case Symbol:
        return d.String
    case FuncionType:
        return fmt.Sprintf("<function: %s>", String(Car(d.FuncionValue)))
    case PrimativeType:
        return d.Primative.String()
    }
}

func Eval(d *Data) *Data {
    if d == nil {
        return nil
    }

    switch d.Type {
    case ConsCellType:
        {
            function = Eval(Car(d))
            args = Cdr(d)
            return Apply(function, args)
        }
    case Symbol:
        return ValueOf(d)
    case NumberType, BooleanType, StringType:
        return d
    }
}

func Apply(function *Data, args *Data) *Data {
}
