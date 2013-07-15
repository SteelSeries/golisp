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
    AlistType
    AlistCellType
    NumberType
    BooleanType
    StringType
    SymbolType
    FunctionType
    PrimitiveType
    ObjectType
)

type Data struct {
    Type    int                // data type
    Car     *Data              // ConsCellType & AlistType
    Cdr     *Data              // ConsCellType & AlistType
    String  string             // StringType & SymbolType
    Number  uint32             // NumberType & BooleanType
    Func    *Function          // FunctionType
    Prim    *PrimitiveFunction // PrimitiveType
    ObjType string             // ObjectType
    Obj     unsafe.Pointer     // ObjectType
}

// Boolean constants

var True *Data = BooleanWithValue(true)
var False *Data = BooleanWithValue(false)

func TypeOf(d *Data) int {
    return d.Type
}

func NilP(d *Data) bool {
    if d == nil {
        return true
    }
    if (PairP(d) || AlistP(d) || DottedPairP(d)) && Car(d) == nil && Cdr(d) == nil {
        return true
    }
    return false
}

func NotNilP(d *Data) bool {
    return d != nil
}

func PairP(d *Data) bool {
    return d == nil || TypeOf(d) == ConsCellType
}

func DottedPairP(d *Data) bool {
    return d == nil || TypeOf(d) == AlistCellType
}

func AlistP(d *Data) bool {
    return d == nil || TypeOf(d) == AlistType
}

func SymbolP(d *Data) bool {
    return d != nil && TypeOf(d) == SymbolType
}

func StringP(d *Data) bool {
    return d != nil && TypeOf(d) == StringType
}

func NumberP(d *Data) bool {
    return d != nil && TypeOf(d) == NumberType
}

func ObjectP(d *Data) bool {
    return d != nil && TypeOf(d) == ObjectType
}

func FunctionP(d *Data) bool {
    return d != nil && TypeOf(d) == FunctionType || TypeOf(d) == PrimitiveType
}

func Cons(car *Data, cdr *Data) *Data {
    return &Data{Type: ConsCellType, Car: car, Cdr: cdr, String: "", Number: 0, Func: nil, Prim: nil}
}

func Append(l *Data, value *Data) *Data {
    if NilP(l) {
        return Cons(value, nil)
    }

    if Cdr(l) == nil {
        l.Cdr = Cons(value, nil)
    } else {
        Append(Cdr(l), value)
    }

    return l
}

func AppendList(l *Data, otherList *Data) *Data {
    if NilP(l) {
        return otherList
    }

    var c *Data
    for c = l; NotNilP(c.Cdr); c = Cdr(c) {
    }

    c.Cdr = otherList

    return l
}

func Acons(car *Data, cdr *Data, alist *Data) *Data {
    pair, _ := Assoc(car, alist)
    if NilP(pair) {
        cell := &Data{Type: AlistCellType, Car: car, Cdr: cdr, String: "", Number: 0, Func: nil, Prim: nil}
        return &Data{Type: AlistType, Car: cell, Cdr: alist, String: "", Number: 0, Func: nil, Prim: nil}
    } else {
        pair.Cdr = cdr
        return alist
    }
}

func Alist(d *Data) *Data {
    if NilP(d) {
        return nil
    }

    if PairP(d) {
        return Acons(Caar(d), Cdar(d), Alist(Cdr(d)))
    }

    return d
}

func InternalMakeList(c ...*Data) *Data {
    return ArrayToList(c)
}

func EmptyCons() *Data {
    return Cons(nil, nil)
}

func NumberWithValue(n uint32) *Data {
    return &Data{Type: NumberType, Car: nil, Cdr: nil, String: "", Number: n, Func: nil, Prim: nil}
}

func BooleanWithValue(b bool) *Data {
    var num int = 0
    if b {
        num = 1
    }
    return &Data{Type: BooleanType, Car: nil, Cdr: nil, String: "", Number: uint32(num), Func: nil, Prim: nil}
}

func StringWithValue(s string) *Data {
    return &Data{Type: StringType, Car: nil, Cdr: nil, String: s, Number: 0, Func: nil, Prim: nil}
}

func SymbolWithName(s string) *Data {
    return &Data{Type: SymbolType, Car: nil, Cdr: nil, String: s, Number: 0, Func: nil, Prim: nil}
}

func FunctionWithNameParamsBodyAndParent(name string, params *Data, body *Data, parentEnv *SymbolTableFrame) *Data {
    return &Data{Type: FunctionType, Car: nil, Cdr: nil, String: "", Number: 0, Func: MakeFunction(name, params, body, parentEnv), Prim: nil}
}

func PrimitiveWithNameAndFunc(name string, f *PrimitiveFunction) *Data {
    return &Data{Type: PrimitiveType, Car: nil, Cdr: nil, String: "", Number: 0, Func: nil, Prim: f}
}

func ObjectWithTypeAndValue(typeName string, o unsafe.Pointer) *Data {
    return &Data{Type: ObjectType, Car: nil, Cdr: nil, String: "", Number: 0, Func: nil, Prim: nil, ObjType: typeName, Obj: o}
}

func NumericValue(d *Data) uint32 {
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

func TypeOfObject(d *Data) (oType string) {
    if d == nil {
        return
    }

    if d.Type == ObjectType {
        return d.ObjType
    }

    return
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

    if d.Type == ConsCellType || d.Type == AlistType {
        return 1 + Length(d.Cdr)
    }

    return 0
}

func Reverse(d *Data) (result *Data) {
    if d == nil {
        return nil
    }

    if !PairP(d) {
        return d
    }

    var l *Data = nil
    for c := d; NotNilP(c); c = Cdr(c) {
        l = Cons(Car(c), l)
    }

    return l
}

func Assoc(key *Data, alist *Data) (result *Data, err error) {
    for c := alist; NotNilP(c); c = Cdr(c) {
        pair := Car(c)
        if !DottedPairP(pair) && !PairP(pair) {
            err = errors.New("Alist key can not be a list")
            return
        }
        if IsEqual(Car(pair), key) {
            result = pair
            return
        }
    }
    return
}

func IsEqual(d *Data, o *Data) bool {
    if d == o {
        return true
    }

    if d == nil || o == nil {
        return false
    }

    if AlistP(d) {
        if !AlistP(o) && !PairP(o) {
            return false
        }
    } else if TypeOf(d) == AlistCellType {
        if TypeOf(o) != ConsCellType && TypeOf(o) != AlistCellType {
            return false
        }
    } else if TypeOf(o) != TypeOf(d) {
        return false
    }

    if PairP(d) {
        if Length(d) != Length(o) {
            return false
        }
        for a1, a2 := d, o; NotNilP(a1); a1, a2 = Cdr(a1), Cdr(a2) {
            if !IsEqual(Car(a1), Car(a2)) {
                return false
            }
        }
        return true
    }

    if AlistP(d) {
        if Length(d) != Length(o) {
            return false
        }
        for c := d; NotNilP(c); c = Cdr(c) {
            otherPair, err := Assoc(Caar(c), o)
            if err != nil || NilP(otherPair) || !IsEqual(Cdar(c), Cdr(otherPair)) {
                return false
            }
        }
        return true
    }

    if TypeOf(d) == AlistCellType {
        return IsEqual(Car(d), Car(o)) && IsEqual(Cdr(d), Cdr(o))
    }

    return *d == *o
}

func escapeQuotes(str string) string {
    buffer := make([]rune, 0, 10)
    for _, ch := range str {
        if rune(ch) == '"' {
            buffer = append(buffer, '\\')
        }
        buffer = append(buffer, rune(ch))
    }
    return string(buffer)
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
            for NotNilP(c) && PairP(c) {
                contents = append(contents, String(Car(c)))
                c = Cdr(c)
            }
            if c == nil {
                if SymbolP(Car(d)) && StringValue(Car(d)) == "quote" {
                    if len(contents) == 1 {
                        return fmt.Sprintf("'()")
                    } else {
                        return fmt.Sprintf("'%s", contents[1])
                    }
                } else {
                    return fmt.Sprintf("(%s)", strings.Join(contents, " "))
                }
            } else {
                return fmt.Sprintf("(%s . %s)", strings.Join(contents, " "), String(c))
            }
        }
    case AlistType:
        {
            if NilP(Car(d)) && NilP(Cdr(d)) {
                return "()"
            }
            contents := make([]string, 0, Length(d))
            for c := d; NotNilP(c); c = Cdr(c) {
                contents = append(contents, String(Car(c)))
            }
            return fmt.Sprintf("(%s)", strings.Join(contents, " "))
        }
    case AlistCellType:
        return fmt.Sprintf("(%s . %s)", String(Car(d)), String(Cdr(d)))
    case NumberType:
        return fmt.Sprintf("%d", d.Number)
    case BooleanType:
        if d.Number == 0 {
            return "#f"
        } else {
            return "#t"
        }
    case StringType:
        return fmt.Sprintf(`"%s"`, escapeQuotes(d.String))
    case SymbolType:
        return d.String
    case FunctionType:
        return fmt.Sprintf("<function: %s>", d.Func.Name)
    case PrimitiveType:
        return d.Prim.String()
    case ObjectType:
        return fmt.Sprintf("<opaque Go object of type %s : 0x%x>", d.ObjType, (*uint64)(d.Obj))
    }

    return ""
}

func Eval(d *Data, env *SymbolTableFrame) (result *Data, err error) {
    if d == nil {
        return
    }

    switch d.Type {
    case ConsCellType:
        {
            var function *Data
            function, err = Eval(Car(d), env)
            if err != nil {
                return
            }
            args := Cdr(d)
            result, err = Apply(function, args, env)
            if err != nil {
                err = errors.New(fmt.Sprintf("Evaling %s. %s", String(d), err))
                return
            }
            return
        }
    case SymbolType:
        result = env.ValueOf(d)
        return
    }

    return d, nil
}

func Apply(function *Data, args *Data, env *SymbolTableFrame) (result *Data, err error) {
    if function == nil {
        err = errors.New("Nil when function expected.")
        return
    }
    switch function.Type {
    case FunctionType:
        return function.Func.Apply(args, env)
    case PrimitiveType:
        return function.Prim.Apply(args, env)
    }
    return
}
