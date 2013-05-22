// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file defines functions for device support
package golisp

// (def-struct <name> <field>...)
// (def-field <name> <type> [<count>] [<to json translation> <from json translation>])
// (def-api <struct name> (read ) (write ))

import (
    "errors"
    //    . "github.com/steelseries/golisp"
    "fmt"
    "unsafe"
)

var currentStructure *DeviceStructure
var currentField *DeviceField

func init() {
    //    InitDeviceBuiltins()
}

func InitDeviceBuiltins() {
    MakePrimitiveFunction("def-struct", -1, DefStruct)
    MakePrimitiveFunction("def-field", -1, DefField)
    MakePrimitiveFunction("range", 2, DefRange)
    MakePrimitiveFunction("values", -1, DefValues)
    MakePrimitiveFunction("repeat", 1, DefRepeat)
    MakePrimitiveFunction("to-json", 1, DefToJson)
    MakePrimitiveFunction("to-from", 1, DefFromJson)

    MakePrimitiveFunction("def-api", -1, DefApi)
    MakePrimitiveFunction("dump-struct", 1, DumpStructure)
    MakePrimitiveFunction("dump-expanded", 1, DumpExpanded)

    MakePrimitiveFunction("bytes-to-string", 2, BytesToString)
    MakePrimitiveFunction("string-to-bytes", 2, StringToBytes)
}

func DefStruct(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    structName := Car(args)
    if TypeOf(structName) != SymbolType {
        err = errors.New("Struct name must be a symbol")
        return
    }

    if NilP(Cdr(args)) {
        err = errors.New("Struct must have at least one field")
        return
    }

    fmt.Printf("device structure: %s\n", StringValue(structName))
    structure := NewStruct(StringValue(structName))
    currentStructure = structure
    currentField = nil

    var field *Data
    for c := Cdr(args); NotNilP(c); c = Cdr(c) {
        field, err = Eval(Car(c), env)
        if err != nil {
            return
        }
        if ObjectP(field) && TypeOfObject(field) == "DeviceField" {
            structure.AddField((*DeviceField)(ObjectValue(field)))
        } else {
            errors.New("Expected DeviceField")
        }
    }
    currentStructure = nil
    return env.BindTo(structName, ObjectWithTypeAndValue("DeviceStructure", unsafe.Pointer(structure))), nil
}

func DefField(args *Data, env *SymbolTableFrame) (field *Data, err error) {
    fieldName := Car(args)
    if TypeOf(fieldName) != SymbolType {
        err = errors.New("Field name must be a symbol")
        return
    }

    if NilP(Cdr(args)) {
        err = errors.New("Field must have at least a name and type")
        return
    }

    fieldType := Cadr(args)
    if TypeOf(fieldType) != SymbolType {
        err = errors.New("Field type must be a symbol")
        return
    }

    if !IsValidType(fieldType) {
        err = errors.New("Field type must be uint8, uint16, uint32 or a user defined device structure")
        return
    }

    currentField = NewField(StringValue(fieldName), StringValue(fieldType), FieldSizeOf(fieldType))

    for c := Cddr(args); NotNilP(c); c = Cdr(c) {
        field, err = Eval(Car(c), env)
        if err != nil {
            return
        }
    }

    field = ObjectWithTypeAndValue("DeviceField", unsafe.Pointer(currentField))
    currentField = nil
    return
}

func DefRange(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    if Length(args) != 2 {
        err = errors.New(fmt.Sprintf("range requires 2 arguments, %d found", Length(args)))
        return
    }

    if !NumberP(Car(args)) || !NumberP(Cadr(args)) {
        err = errors.New("range requires numeric arguments")
        return
    }

    currentField.ValidRange = Range{Lo: uint32(IntValue(Car(args))), Hi: uint32(IntValue(Cadr(args)))}
    return
}

func DefValues(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    if Length(args) == 0 {
        err = errors.New("values requires at least 1 argument")
        return
    }

    currentField.ValidValues = make([]uint32, 0, Length(args))
    for c := args; NotNilP(c); c = Cdr(c) {
        if !NumberP(Car(c)) {
            err = errors.New("values requires numeric arguments")
            return
        }

        currentField.ValidValues = append(currentField.ValidValues, uint32(IntValue(Car(c))))
    }
    return
}

func DefRepeat(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    if Length(args) != 1 {
        err = errors.New(fmt.Sprintf("repeat requires 1 argument, %d found", Length(args)))
        return
    }

    if !NumberP(Car(args)) {
        err = errors.New("repeat requires a numeric argument")
        return
    }

    currentField.RepeatCount = int(IntValue(Car(args)))
    return
}

func DefToJson(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    if Length(args) != 1 {
        err = errors.New(fmt.Sprintf("to-json requires 1 argument, %d found", Length(args)))
        return
    }

    if !PairP(Car(args)) {
        err = errors.New("to-json requires a list argument")
        return
    }

    currentField.ToJsonTransform = Car(args)
    return
}

func DefFromJson(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    if Length(args) != 1 {
        err = errors.New(fmt.Sprintf("from-json requires 1 argument, %d found", Length(args)))
        return
    }

    if !PairP(Car(args)) {
        err = errors.New("from-json requires a list argument")
        return
    }

    currentField.FromJsonTransform = Car(args)
    return
}

func DefApi(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    return
}

func DumpStructure(d *Data, env *SymbolTableFrame) (result *Data, err error) {
    structObj, err := Eval(Car(d), env)
    if err != nil {
        return
    }

    if ObjectP(structObj) && TypeOfObject(structObj) == "DeviceStructure" {
        structure := ((*DeviceStructure)(ObjectValue(structObj)))
        structure.Dump()
    } else {
        err = errors.New("dump-structure expected DeviceStructure")
    }
    return
}

func DumpExpanded(d *Data, env *SymbolTableFrame) (result *Data, err error) {
    structObj, err := Eval(Car(d), env)
    if err != nil {
        return
    }

    if ObjectP(structObj) && TypeOfObject(structObj) == "DeviceStructure" {
        structure := ((*DeviceStructure)(ObjectValue(structObj)))
        structure.DumpExpanded()
    } else {
        err = errors.New("dump-expanded expected DeviceStructure")
    }
    return
}

func CharsToString(ca []uint8) string {
    s := make([]byte, len(ca))
    var lens int
    for ; lens < len(ca); lens++ {
        if ca[lens] == 0 {
            break
        }
        s[lens] = uint8(ca[lens])
    }
    return string(s[0:lens])
}

func BytesToString(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    node := Car(args)
    parent := Cadr(args)

    var bytes [16]uint8
    for c, i := node, 0; NotNilP(c); c, i = Cdr(c), i+1 {
        bytes[i] = uint8(IntValue(Car(c)))
    }

    str := CharsToString(bytes[0:16])
    Acons(StringWithValue("name"), StringWithValue(str), parent)
    return
}

func StringToBytes(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    node := Car(args)
    parent := Cadr(args)
    var name [16]*Data
    for i, b := range []byte(StringValue(node)) {
        name[i] = NumberWithValue(int(b))
    }
    for j := len(StringValue(node)); j < 16; j++ {
        name[j] = NumberWithValue(0)
    }

    ary := ArrayToList(name[0:16])
    Acons(StringWithValue("name"), ary, parent)

    return
}
