// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file inmpliments data elements
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
    PrimitiveType
)

type Data struct {
    Type         int
    Car          *Data
    Cdr          *Data
    String       string
    Number       int
    FuncionValue *Data
    Primitive    *PrimitiveFunction
}

var True *Data = BooleanWithValue(true)
var False *Data = BooleanWithValue(false)

func TypeOf(d *Data) int {
    return d.Type
}

func Cons(car *Data, cdr *Data) *Data {
    return &Data{Type: ConsCellType, Car: car, Cdr: cdr, String: "", Number: 0}
}

func EmptyCons() *Data {
    return Cons(nil, nil)
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

func SymbolWithName(s string) *Data {
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

    return true
}

func Length(d *Data) int {
    if d == nil {
        return 0
    }

    if d.Type == ConsCellType {
        return 1 + Length(d.Cdr)
    }

    return 0
}

func IsEqual(d *Data, o *Data) bool {
    return d == o || *d == *o
}

func NilP(d *Data) bool {
    return d == nil
}

func NotNilP(d *Data) bool {
    return d != nil
}

func ListP(d *Data) bool {
    return d.Type == ConsCellType
}

func String(d *Data) string {
    if d == nil {
        return "()"
    }

    switch d.Type {
    case ConsCellType:
        {
            if NilP(Car(d)) && NilP(Cdr(d)) {
                return "()"
            }
            var c *Data = d
            contents := make([]string, 0, Length(d))
            for NotNilP(c) && ListP(c) {
                contents = append(contents, String(Car(c)))
                c = Cdr(c)
            }
            if NilP(c) {
                return fmt.Sprintf("(%s)", strings.Join(contents, " "))
            } else {
                return fmt.Sprintf("(%s . %s)", strings.Join(contents, " "), String(c))
            }
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
        return fmt.Sprintf(`"%s"`, d.String)
    case SymbolType:
        return d.String
    case FunctionType:
        return fmt.Sprintf("<function: %s>", String(Car(d.FuncionValue)))
    case PrimitiveType:
        return d.Primitive.String()
    }

    return ""
}

func Eval(d *Data) *Data {
    if d == nil {
        return nil
    }

    switch d.Type {
    case ConsCellType:
        {
            function := Eval(Car(d))
            args := Cdr(d)
            return Apply(function, args)
        }
    case SymbolType:
        return ValueOf(d)
    case NumberType, BooleanType, StringType:
        return d
    }

    return nil
}

func Apply(function *Data, args *Data) *Data {
    return nil
}
