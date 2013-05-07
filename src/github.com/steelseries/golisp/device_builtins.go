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
    "unsafe"
)

var currentStructure *DeviceStructure

func init() {
    //    InitDeviceBuiltins()
}

func InitDeviceBuiltins() {
    MakePrimitiveFunction("def_struct", -1, DefStruct)
    MakePrimitiveFunction("def_field", -1, DefField)
    MakePrimitiveFunction("def_api", -1, DefApi)
    MakePrimitiveFunction("dump_struct", 1, DumpStructure)
    MakePrimitiveFunction("dump_expanded", 1, DumpExpanded)
}

func DefStruct(args *Data) (result *Data, err error) {
    structName := Car(args)
    if TypeOf(structName) != SymbolType {
        err = errors.New("Struct name must be a symbol")
        return
    }

    if NilP(Cdr(args)) {
        err = errors.New("Struct must have at least one field")
        return
    }

    structure := NewStruct(StringValue(structName))
    currentStructure = structure

    var field *Data
    for c := Cdr(args); NotNilP(c); c = Cdr(c) {
        field, err = Eval(Car(c))
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
    return BindTo(structName, ObjectWithTypeAndValue("DeviceStructure", unsafe.Pointer(structure))), nil
}

func DefField(args *Data) (field *Data, err error) {
    fieldName := First(args)
    if TypeOf(fieldName) != SymbolType {
        err = errors.New("Field name must be a symbol")
        return
    }

    if NilP(Cdr(args)) {
        err = errors.New("Field must have at least a name and type")
        return
    }

    fieldType := Second(args)
    if TypeOf(fieldType) != SymbolType {
        err = errors.New("Field type must be a symbol")
        return
    }

    if !IsValidType(fieldType) {
        err = errors.New("Field type must be uint8, uint16, uint32 or a user defined device structure")
        return
    }

    var fieldCount int = 1
    var nextArg int = 3
    if NumberP(Third(args)) {
        fieldCount = IntValue(Third(args))
        nextArg = 4
    }

    var toXform *Data
    var fromXform *Data
    if NotNilP(Nth(args, nextArg)) {
        toXform = Nth(args, nextArg)
        nextArg += 1
    }
    if NotNilP(Nth(args, nextArg)) {
        fromXform = Nth(args, nextArg)
    }

    size := FieldSizeOf(fieldType)

    field = ObjectWithTypeAndValue("DeviceField", unsafe.Pointer(NewField(StringValue(fieldName), StringValue(fieldType), size, fieldCount, toXform, fromXform)))
    return
}

func DefApi(args *Data) (result *Data, err error) {
    return
}

func DumpStructure(d *Data) (result *Data, err error) {
    structObj, err := Eval(Car(d))
    if err != nil {
        return
    }

    if ObjectP(structObj) && TypeOfObject(structObj) == "DeviceStructure" {
        structure := ((*DeviceStructure)(ObjectValue(structObj)))
        structure.Dump()
    } else {
        err = errors.New("Expected DeviceStructure")
    }
    return
}

func DumpExpanded(d *Data) (result *Data, err error) {
    structObj, err := Eval(Car(d))
    if err != nil {
        return
    }

    if ObjectP(structObj) && TypeOfObject(structObj) == "DeviceStructure" {
        structure := ((*DeviceStructure)(ObjectValue(structObj)))
        structure.DumpExpanded()
    } else {
        err = errors.New("Expected DeviceStructure")
    }
    return
}
