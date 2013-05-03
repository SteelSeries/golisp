// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file inmpliments data elements
package golisp

import (
    "errors"
    "fmt"
    "strings"
    "unsafe"
)

const (
    ConsCellType = iota
    NumberType
    BooleanType
    StringType
    SymbolType
    FunctionType
    PrimitiveType
    ObjectType
)

type Data struct {
    Type   int                // data type
    Car    *Data              // ConsCellType
    Cdr    *Data              // ConsCellType
    String string             // StringType & SymbolType
    Number int                // NumberType & BooleanType
    Func   *Function          // FunctionType
    Prim   *PrimitiveFunction // PrimitiveType
    Obj    unsafe.Pointer     // ObjectType
}

// Boolean constants

var True *Data = BooleanWithValue(true)
var False *Data = BooleanWithValue(false)

func TypeOf(d *Data) int {
    return d.Type
}

func SymbolP(d *Data) bool {
    return TypeOf(d) == SymbolType
}

func NumberP(d *Data) bool {
    return TypeOf(d) == NumberType
}

func PairP(d *Data) bool {
    return TypeOf(d) == ConsCellType
}

func Cons(car *Data, cdr *Data) *Data {
    return &Data{Type: ConsCellType, Car: car, Cdr: cdr, String: "", Number: 0, Func: nil, Prim: nil}
}

func EmptyCons() *Data {
    return Cons(nil, nil)
}

func NumberWithValue(n int) *Data {
    return &Data{Type: NumberType, Car: nil, Cdr: nil, String: "", Number: n, Func: nil, Prim: nil}
}

func BooleanWithValue(b bool) *Data {
    var num int = 0
    if b {
        num = 1
    }
    return &Data{Type: BooleanType, Car: nil, Cdr: nil, String: "", Number: num, Func: nil, Prim: nil}
}

func StringWithValue(s string) *Data {
    return &Data{Type: StringType, Car: nil, Cdr: nil, String: s, Number: 0, Func: nil, Prim: nil}
}

func SymbolWithName(s string) *Data {
    return &Data{Type: SymbolType, Car: nil, Cdr: nil, String: s, Number: 0, Func: nil, Prim: nil}
}

func FunctionWithNameParamsAndBody(name string, params *Data, body *Data) *Data {
    return &Data{Type: FunctionType, Car: nil, Cdr: nil, String: "", Number: 0, Func: MakeFunction(name, params, body), Prim: nil}
}

func PrimitiveWithNameAndFunc(name string, f *PrimitiveFunction) *Data {
    return &Data{Type: PrimitiveType, Car: nil, Cdr: nil, String: "", Number: 0, Func: nil, Prim: f}
}

func ObjectWithValue(o unsafe.Pointer) *Data {
    return &Data{Type: ObjectType, Car: nil, Cdr: nil, String: "", Number: 0, Func: nil, Prim: nil, Obj: o}
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

func ObjectValue(d *Data) (p unsafe.Pointer) {
    if d == nil {
        return
    }

    if d.Type == ObjectType {
        return d.Obj
    }

    return
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
        return fmt.Sprintf("<function: %s>", d.Func.Name)
    case PrimitiveType:
        return d.Prim.String()
    case ObjectType:
        return fmt.Sprintf("<opaque Go object: %v>", d.Obj)
    }

    return ""
}

func Eval(d *Data) (result *Data, err error) {
    if d == nil {
        return
    }

    switch d.Type {
    case ConsCellType:
        {
            var function *Data
            function, err = Eval(Car(d))
            if err != nil {
                return
            }
            args := Cdr(d)
            result, err = Apply(function, args)
            return
        }
    case SymbolType:
        result = ValueOf(d)
        return
    }

    return d, nil
}

func Apply(function *Data, args *Data) (result *Data, err error) {
    if function == nil {
        err = errors.New("Nil when function expected.\n")
        return
    }
    switch function.Type {
    case FunctionType:
        return function.Func.Apply(args)
    case PrimitiveType:
        return function.Prim.Apply(args)
    }
    return
}
