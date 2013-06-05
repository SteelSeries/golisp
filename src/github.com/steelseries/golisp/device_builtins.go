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

var CurrentDevice *DeviceDeclaration
var CurrentStructure *DeviceStructure
var CurrentField *DeviceField
var CurrentDirection uint

const (
    HID_PROTOCOL = iota
    X2_PROTOCOL
    KNX_PROTOCOL
    SSE_PROTOCOL
)

func init() {
    //    InitDeviceBuiltins()
    InitProtocolConstants()
}

func InitProtocolConstants() {
    Global.BindTo(SymbolWithName("HID"), NumberWithValue(HID_PROTOCOL))
    Global.BindTo(SymbolWithName("X2"), NumberWithValue(X2_PROTOCOL))
    Global.BindTo(SymbolWithName("KNX"), NumberWithValue(KNX_PROTOCOL))
    Global.BindTo(SymbolWithName("SSE"), NumberWithValue(SSE_PROTOCOL))
}

func InitDeviceBuiltins() {
    MakePrimitiveFunction("def-device", -1, DefDevice)

    MakePrimitiveFunction("def-struct", -1, DefStruct)
    MakePrimitiveFunction("def-field", -1, DefField)
    MakePrimitiveFunction("outgoing", -1, DefOutgoing)
    MakePrimitiveFunction("incoming", -1, DefIncoming)
    MakePrimitiveFunction("common", -1, DefCommon)
    MakePrimitiveFunction("constant", 1, DefConstant)
    MakePrimitiveFunction("range", 2, DefRange)
    MakePrimitiveFunction("values", -1, DefValues)
    MakePrimitiveFunction("deferred-validation", -1, DefDeferredValidation)
    MakePrimitiveFunction("repeat", 1, DefRepeat)
    MakePrimitiveFunction("to-json", 1, DefToJson)
    MakePrimitiveFunction("to-from", 1, DefFromJson)

    MakePrimitiveFunction("def-api", -1, DefApi)
    MakePrimitiveFunction("read", -1, ApiRead)
    MakePrimitiveFunction("write", -1, ApiWrite)
    MakePrimitiveFunction("chunk", 3, DefChunk)

    MakePrimitiveFunction("dump-struct", 1, DumpStructure)
    MakePrimitiveFunction("dump-expanded", 1, DumpExpanded)

    MakePrimitiveFunction("bytes-to-string", 2, BytesToString)
    MakePrimitiveFunction("string-to-bytes", 2, StringToBytes)
    MakePrimitiveFunction("list-to-bytearray", 1, ListToBytes)
    MakePrimitiveFunction("replace-byte", 3, ReplaceByte)
}

func DefDevice(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    deviceName := Car(args)
    if TypeOf(deviceName) != SymbolType {
        err = errors.New("Device name must be a symbol")
        return
    }

    if NilP(Cdr(args)) {
        err = errors.New("Device must have at least one structure")
        return
    }

    CurrentDevice := NewDeviceNamed(StringValue(deviceName))
    deviceEnv := NewSymbolTableFrameBelow(env)
    CurrentDevice.Env = deviceEnv

    var thing *Data
    for c := Cdr(args); NotNilP(c); c = Cdr(c) {
        thing, err = Eval(Car(c), deviceEnv)
        if err != nil {
            return
        }
        if ObjectP(thing) && TypeOfObject(thing) == "DeviceStructure" {
            CurrentDevice.AddStructure((*DeviceStructure)(ObjectValue(thing)))
        } else if ObjectP(thing) && TypeOfObject(thing) == "DeviceApi" {
            CurrentDevice.AddApi((*DeviceApi)(ObjectValue(thing)))
        } else {
            errors.New("Expected structure declaration or api declaration")
        }
    }
    CurrentStructure = nil
    return Global.BindTo(deviceName, ObjectWithTypeAndValue("DeviceDeclaration", unsafe.Pointer(CurrentDevice))), nil
}

func DefStruct(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    structName := Car(args)
    if TypeOf(structName) != SymbolType {
        err = errors.New("Struct name must be a symbol")
        return
    }

    if NilP(Cdr(args)) {
        err = errors.New("Struct must have at least one section")
        return
    }

    structure := NewStructNamed(StringValue(structName))
    CurrentStructure = structure

    for c := Cdr(args); NotNilP(c); c = Cdr(c) {
        _, err = Eval(Car(c), env)
        if err != nil {
            return
        }
    }

    CurrentStructure = nil
    return Global.BindTo(structName, ObjectWithTypeAndValue("DeviceStructure", unsafe.Pointer(structure))), nil
}

func ProcessStructureDefinition(args *Data, env *SymbolTableFrame, direction uint) (def *DeviceStructureDefinition, err error) {
    def = NewDeviceStructureDefinition()
    CurrentField = nil
    CurrentDirection = direction

    var field *Data
    for c := args; NotNilP(c); c = Cdr(c) {
        field, err = Eval(Car(c), env)
        if err != nil {
            return
        }
        if ObjectP(field) && TypeOfObject(field) == "DeviceField" {
            def.AddField((*DeviceField)(ObjectValue(field)))
        } else {
            err = errors.New("Expected DeviceField")
            return
        }
    }
    return
}

func DefOutgoing(args *Data, env *SymbolTableFrame) (field *Data, err error) {
    CurrentStructure.Outgoing, err = ProcessStructureDefinition(args, env, outgoing)
    return
}

func DefIncoming(args *Data, env *SymbolTableFrame) (field *Data, err error) {
    CurrentStructure.Incoming, err = ProcessStructureDefinition(args, env, incoming)
    return
}

func DefCommon(args *Data, env *SymbolTableFrame) (field *Data, err error) {
    def, err := ProcessStructureDefinition(args, env, outgoing)
    CurrentStructure.Outgoing = def
    CurrentStructure.Incoming = def
    return
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

    CurrentField = NewField(StringValue(fieldName), StringValue(fieldType), FieldSizeOf(fieldType, CurrentDirection))

    for c := Cddr(args); NotNilP(c); c = Cdr(c) {
        field, err = Eval(Car(c), env)
        if err != nil {
            return
        }
    }

    field = ObjectWithTypeAndValue("DeviceField", unsafe.Pointer(CurrentField))
    CurrentField = nil
    return
}

func DefConstant(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    if !NumberP(Car(args)) {
        err = errors.New("constant requires a numeric argument")
        return
    }
    CurrentField.IsConstant = true
    CurrentField.Constant = uint32(NumericValue(Car(args)))
    return
}

func DefRange(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    if !NumberP(Car(args)) || !NumberP(Cadr(args)) {
        err = errors.New("range requires numeric arguments")
        return
    }

    lo := uint32(NumericValue(Car(args)))
    hi := uint32(NumericValue(Cadr(args)))

    if lo >= hi {
        err = errors.New("range requires arguments in ascending order")
        return
    }

    CurrentField.ValidRange = &Range{Lo: lo, Hi: hi}
    return
}

func DefValues(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    if Length(args) == 0 {
        err = errors.New("values requires at least 1 argument")
        return
    }

    CurrentField.ValidValues = &Values{Vals: make([]uint32, 0, 5)}

    var l *Data
    if PairP(Car(args)) {
        l, err = Eval(Car(args), env)
        if err != nil {
            return
        }
        args = l
    }

    for c := args; NotNilP(c); c = Cdr(c) {
        if !NumberP(Car(c)) {
            err = errors.New("values requires numeric arguments")
            return
        }
        CurrentField.ValidValues.AddValue(uint32(NumericValue(Car(c))))
    }
    return
}

func DefDeferredValidation(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    CurrentField.DeferredValidationCode = Car(args)
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

    CurrentField.RepeatCount = int(NumericValue(Car(args)))
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

    CurrentField.ToJsonTransform = Car(args)
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

    CurrentField.FromJsonTransform = Car(args)
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
        bytes[i] = uint8(NumericValue(Car(c)))
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
        name[i] = NumberWithValue(uint32(b))
    }
    for j := len(StringValue(node)); j < 16; j++ {
        name[j] = NumberWithValue(0)
    }

    ary := ArrayToList(name[0:16])
    Acons(StringWithValue("name"), ary, parent)

    return
}

func ListToBytes(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    list, err := Eval(Car(args), env)
    if err != nil {
        return
    }

    bytes := make([]byte, 0, int(Length(list)))
    for c := list; NotNilP(c); c = Cdr(c) {
        if !NumberP(Car(c)) {
            err = errors.New("Byte arrays can only contain bytes.")
            return
        }
        b := NumericValue(Car(c))
        if b > 255 {
            err = errors.New("Byte arrays can only contain bytes.")
            return
        }
        bytes = append(bytes, byte(b))
    }
    return ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&bytes)), nil
}

func ReplaceByte(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    dataByteObject, err := Eval(Car(args), env)
    if err != nil {
        panic(err)
    }
    if !ObjectP(dataByteObject) || TypeOfObject(dataByteObject) != "[]byte" {
        panic(errors.New(fmt.Sprintf("Bytearray object should return []byte but returned %s.", TypeOfObject(dataByteObject))))
    }

    dataBytes := (*[]byte)(ObjectValue(dataByteObject))

    indexObject, err := Eval(Cadr(args), env)
    if err != nil {
        panic(err)
    }
    if !NumberP(indexObject) {
        panic(errors.New("Bytearray index should be a number."))
    }
    index := int(NumberValue(indexObject))

    valueObject, err := Eval(Caddr(args), env)
    if err != nil {
        panic(err)
    }
    if !NumberP(valueObject) {
        panic(errors.New("Bytearray value should be a number."))
    }

    value := byte(NumberValue(valueObject))

    (*dataBytes)[index] = value

    &ObjectWithTypeAndValue("byte[]", unsafe.Pointer(dataBytes))
}
