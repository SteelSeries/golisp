// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file implements data elements.

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
    FloatType
    BooleanType
    StringType
    SymbolType
    FunctionType
    PrimitiveType
    ObjectType
)

type Data struct {
    Type    int    // data type
    Car     *Data  // ConsCellType & AlistType
    Cdr     *Data  // ConsCellType & AlistType
    String  string // StringType & SymbolType
    Number  uint32 // NumberType & BooleanType
    Float   float32
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

func TypeName(t int) string {
    switch t {
    case ConsCellType:
        return "List"
    case AlistType:
        return "Association List"
    case AlistCellType:
        return "Association List Cell"
    case NumberType:
        return "Integer"
    case FloatType:
        return "Float"
    case BooleanType:
        return "Boolean"
    case StringType:
        return "String"
    case SymbolType:
        return "Symbol"
    case FunctionType:
        return "Function"
    case PrimitiveType:
        return "Primitive"
    case ObjectType:
        return "Go Object"
    default:
        return "Unknown"
    }
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
    return !NilP(d)
}

func PairP(d *Data) bool {
    return d == nil || TypeOf(d) == ConsCellType
}

func ListP(d *Data) bool {
    return PairP(d) || AlistP(d)
}

func DottedPairP(d *Data) bool {
    return d == nil || TypeOf(d) == AlistCellType
}

func AlistP(d *Data) bool {
    return d == nil || TypeOf(d) == AlistType
}

func BooleanP(d *Data) bool {
    return d != nil && TypeOf(d) == BooleanType
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

func FloatP(d *Data) bool {
    return d != nil && TypeOf(d) == FloatType
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

func AppendBang(l *Data, value *Data) *Data {
    if NilP(l) {
        return Cons(value, nil)
    }

    var c *Data
    for c = l; NotNilP(c.Cdr); c = Cdr(c) {
    }

    c.Cdr = Cons(value, nil)

    return l
}

func AppendBangList(l *Data, otherList *Data) *Data {
    if NilP(l) {
        return otherList
    }

    var c *Data
    for c = l; NotNilP(c.Cdr); c = Cdr(c) {
    }

    c.Cdr = otherList

    return l
}

func Append(l *Data, value *Data) *Data {
    if NilP(l) {
        return Cons(value, nil)
    }

    var newList = Copy(l)
    var c *Data
    for c = newList; NotNilP(c.Cdr); c = Cdr(c) {
    }

    c.Cdr = Cons(value, nil)

    return newList
}

func AppendList(l *Data, otherList *Data) *Data {
    if NilP(l) {
        return otherList
    }

    var newList = Copy(l)
    var c *Data
    for c = newList; NotNilP(c.Cdr); c = Cdr(c) {
    }

    c.Cdr = otherList

    return newList
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
        headPair := Car(d)
        return Acons(Car(headPair), Cdr(headPair), Alist(Cdr(d)))
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
    return &Data{Type: NumberType, Number: n}
}

func FloatWithValue(n float32) *Data {
    return &Data{Type: FloatType, Float: n}
}

func BooleanWithValue(b bool) *Data {
    var num int = 0
    if b {
        num = 1
    }
    return &Data{Type: BooleanType, Number: uint32(num)}
}

func StringWithValue(s string) *Data {
    return &Data{Type: StringType, String: s}
}

func SymbolWithName(s string) *Data {
    return &Data{Type: SymbolType, String: s}
}

func FunctionWithNameParamsBodyAndParent(name string, params *Data, body *Data, parentEnv *SymbolTableFrame) *Data {
    return &Data{Type: FunctionType, Func: MakeFunction(name, params, body, parentEnv)}
}

func PrimitiveWithNameAndFunc(name string, f *PrimitiveFunction) *Data {
    return &Data{Type: PrimitiveType, Prim: f}
}

func ObjectWithTypeAndValue(typeName string, o unsafe.Pointer) *Data {
    return &Data{Type: ObjectType, ObjType: typeName, Obj: o}
}

func NumericValue(d *Data) uint32 {
    if d == nil {
        return 0
    }

    if NumberP(d) {
        return d.Number
    }

    if FloatP(d) {
        return uint32(d.Float)
    }

    return 0
}

func FloatValue(d *Data) float32 {
    if d == nil {
        return 0
    }

    if FloatP(d) {
        return d.Float
    }

    if NumberP(d) {
        return float32(d.Number)
    }

    return 0
}

func StringValue(d *Data) string {
    if d == nil {
        return ""
    }

    if StringP(d) || SymbolP(d) {
        return d.String
    }

    return ""
}

func BooleanValue(d *Data) bool {
    if d == nil {
        return false
    }

    if BooleanP(d) {
        return d.Number != 0
    }

    return true
}

func TypeOfObject(d *Data) (oType string) {
    if d == nil {
        return
    }

    if ObjectP(d) {
        return d.ObjType
    }

    return
}

func ObjectValue(d *Data) (p unsafe.Pointer) {
    if d == nil {
        return
    }

    if ObjectP(d) {
        return d.Obj
    }

    return
}

func Length(d *Data) int {
    if d == nil {
        return 0
    }

    if ListP(d) || AlistP(d) {
        return 1 + Length(d.Cdr)
    }

    return 0
}

func Reverse(d *Data) (result *Data) {
    if d == nil {
        return nil
    }

    if !ListP(d) {
        return d
    }

    var l *Data = nil
    for c := d; NotNilP(c); c = Cdr(c) {
        l = Cons(Car(c), l)
    }

    return l
}

func Flatten(d *Data) (result *Data, err error) {
    if d == nil {
        return nil, nil
    }

    if !ListP(d) {
        return d, nil
    }

    var l []*Data = make([]*Data, 0, 10)
    for c := d; NotNilP(c); c = Cdr(c) {
        if ListP(Car(c)) {
            for i := Car(c); NotNilP(i); i = Cdr(i) {
                l = append(l, Car(i))
            }
        } else {
            l = append(l, Car(c))
        }
    }

    return ArrayToList(l), nil
}

func RecursiveFlatten(d *Data) (result *Data, err error) {
    if d == nil {
        return nil, nil
    }

    if !ListP(d) {
        return d, nil
    }

    var l []*Data = make([]*Data, 0, 10)
    var elem *Data
    for c := d; NotNilP(c); c = Cdr(c) {
        if ListP(Car(c)) {
            elem, err = RecursiveFlatten(Car(c))
            if err != nil {
                return
            }
            for i := elem; NotNilP(i); i = Cdr(i) {
                l = append(l, Car(i))
            }
        } else {
            l = append(l, Car(c))
        }
    }

    return ArrayToList(l), nil
}

func QuoteIt(value *Data) (result *Data) {
    return InternalMakeList(SymbolWithName("quote"), value)
}

func Assoc(key *Data, alist *Data) (result *Data, err error) {
    for c := alist; NotNilP(c); c = Cdr(c) {
        pair := Car(c)
        if !DottedPairP(pair) && !PairP(pair) {
            err = errors.New("An alist MUST be made of pairs.")
            return
        }
        if IsEqual(Car(pair), key) {
            result = pair
            return
        }
    }
    return
}

func Copy(d *Data) *Data {
    if d == nil {
        return d
    }

    switch d.Type {
    case AlistType:
        {
            alist := Acons(Copy(Caar(d)), Copy(Cdar(d)), nil)
            for c := Cdr(d); NotNilP(c); c = Cdr(c) {
                alist = Acons(Copy(Caar(c)), Copy(Cdar(c)), alist)
            }
            return alist
        }
    case ConsCellType:
        {
            if NilP(d) {
                return d
            }

            return Cons(Copy(Car(d)), Copy(Cdr(d)))
        }
    }

    return d
}

func IsEqual(d *Data, o *Data) bool {
    if d == o {
        return true
    }

    if d == nil || o == nil {
        return false
    }

    if AlistP(d) {
        if !AlistP(o) && !ListP(o) {
            return false
        }
    } else if DottedPairP(d) {
        if !PairP(o) && !DottedPairP(o) {
            return false
        }
    } else if TypeOf(o) != TypeOf(d) {
        return false
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

    if DottedPairP(d) {
        return IsEqual(Car(d), Car(o)) && IsEqual(Cdr(d), Cdr(o))
    }

    if ListP(d) {
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
    case FloatType:
        {
            raw := fmt.Sprintf("%g", d.Float)
            if strings.ContainsRune(raw, '.') {
                return raw
            }
            return fmt.Sprintf("%s.0", raw)
        }
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
        if d.ObjType == "[]byte" {
            bytes := (*[]byte)(d.Obj)
            contents := make([]string, 0, len(*bytes))
            for _, b := range *bytes {
                contents = append(contents, fmt.Sprintf("%d", b))
            }
            return fmt.Sprintf("[%s]", strings.Join(contents, " "))
        } else {
            return fmt.Sprintf("<opaque Go object of type %s : 0x%x>", d.ObjType, (*uint64)(d.Obj))
        }
    }

    return ""
}

func PrintString(d *Data) string {
    if StringP(d) {
        return d.String
    } else {
        return String(d)
    }
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
            if function == nil {
                err = errors.New(fmt.Sprintf("Nil when function expected for %s.", String(Car(d))))
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

func ApplyWithoutEval(function *Data, args *Data, env *SymbolTableFrame) (result *Data, err error) {
    if function == nil {
        err = errors.New("Nil when function expected.")
        return
    }
    switch function.Type {
    case FunctionType:
        return function.Func.ApplyWithoutEval(args, env)
    case PrimitiveType:
        return function.Prim.ApplyWithoutEval(args, env)
    }
    return
}
