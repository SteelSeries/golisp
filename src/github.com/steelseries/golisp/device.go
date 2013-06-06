// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file implements the device interface.

package golisp

import (
    "encoding/json"
    "errors"
    "fmt"
    "github.com/steelseries/golisp/driver"
    "strconv"
    "strings"
    "unsafe"
)

const (
    outgoing = iota
    incoming
)

var DriverToUse driver.Driver = driver.RealDriver{}

type DeviceDeclaration struct {
    Name               string
    Handle             uint32
    Env                *SymbolTableFrame
    Structures         map[string]*DeviceStructure
    ExpandedStructures map[string]*ExpandedDeviceStructure
    Apis               map[string]*DeviceApi
}

type DeviceStructureDefinition struct {
    Parent *DeviceStructure
    Fields []*DeviceField
    Size   int // size of the struct, in buyes
}

type DeviceStructure struct {
    Name     string
    Parent   *DeviceDeclaration
    Outgoing *DeviceStructureDefinition
    Incoming *DeviceStructureDefinition
}

type Validation interface {
    Validate() bool
}

type Range struct {
    Lo  uint32
    Hi  uint32
}

type Values struct {
    Vals []uint32
}

type DeviceField struct {
    Name                   string
    TypeName               string
    Size                   int // size of a single element, in bytes
    RepeatCount            int // number of elements
    IsConstant             bool
    Constant               uint32
    ValidRange             *Range
    ValidValues            *Values
    DeferredValidationCode *Data
    ToJsonTransform        *Data
    FromJsonTransform      *Data
}

type ExpandedDeviceStructureDefinition struct {
    Parent *ExpandedDeviceStructure
    Fields []*ExpandedField
    Size   int // size of the struct, in bytes
}

type ExpandedDeviceStructure struct {
    Name     string
    Parent   *DeviceDeclaration
    Outgoing *ExpandedDeviceStructureDefinition
    Incoming *ExpandedDeviceStructureDefinition
}

type ExpandedField struct {
    FieldDefinition *DeviceField
    Path            string
    Offset          int
    Value           uint32
    Size            int
}

// helper functions

func IsValidType(typeName *Data) bool {
    typeNameString := StringValue(typeName)
    if typeNameString == "uint8" || typeNameString == "uint16" || typeNameString == "uint32" {
        return true
    }

    typeValue := Global.ValueOf(typeName)
    if !ObjectP(typeValue) || typeValue.ObjType != "DeviceStructure" {
        return false
    }

    fieldType := (*DeviceStructure)(ObjectValue(typeValue))
    return fieldType.Name == typeNameString
}

func FieldSizeOf(typeName *Data, direction uint) int {
    switch StringValue(typeName) {
    case "uint8":
        return 1
    case "uint16":
        return 2
    case "uint32":
        return 4
    default:
        {
            typeValue := Global.ValueOf(typeName)
            fieldType := (*DeviceStructure)(ObjectValue(typeValue))
            var def *DeviceStructureDefinition
            if direction == outgoing {
                def = fieldType.Outgoing
            } else {
                def = fieldType.Incoming
            }
            return def.SizeOf()
        }
    }
}

func IsAtomic(f *DeviceField) bool {
    switch f.TypeName {
    case "uint8", "uint16", "uint32":
        return true
    default:
        return false
    }
}

func AlignmentOf(fieldType string) int {
    switch fieldType {
    case "uint8":
        return 1
    case "uint16":
        return 2
    case "uint32":
        return 4
    default:
        return 1
    }
}

// Device functions
func NewDeviceNamed(n string) (d *DeviceDeclaration) {
    return &DeviceDeclaration{Name: n, Structures: make(map[string]*DeviceStructure, 10), ExpandedStructures: make(map[string]*ExpandedDeviceStructure, 10), Apis: make(map[string]*DeviceApi, 10)}
}

func (self *DeviceDeclaration) AddStructure(s *DeviceStructure) {
    self.Structures[s.Name] = s
}

func (self *DeviceDeclaration) AddApi(a *DeviceApi) {
    self.Apis[a.Name] = a
}

// DeviceStructure functions

func NewStructNamed(n string) (s *DeviceStructure) {
    return &DeviceStructure{Name: n, Parent: CurrentDevice}
}

func NewDeviceStructureDefinition() *DeviceStructureDefinition {
    return &DeviceStructureDefinition{Parent: CurrentStructure, Fields: make([]*DeviceField, 0, 5), Size: 0}
}

func (self *DeviceStructureDefinition) AddField(f *DeviceField) {
    self.Fields = append(self.Fields, f)
    self.Size += f.TotalSize()
}

func (self *DeviceStructureDefinition) SizeOf() int {
    return self.Size
}

func (self *Range) Validate(value uint32) bool {
    return value >= self.Lo && value <= self.Hi
}

func (self *Values) AddValue(value uint32) {
    self.Vals = append(self.Vals, value)
}

func (self *Values) Validate(value uint32) bool {
    for _, v := range self.Vals {
        if v == value {
            return true
        }
    }
    return false
}

// DeviceField functions

func NewFieldWithCount(name string, typeName string, size int, count int) (f *DeviceField) {
    return &DeviceField{Name: name, TypeName: typeName, Size: size, RepeatCount: count}
}

func NewField(name string, typeName string, size int) (f *DeviceField) {
    return &DeviceField{Name: name, TypeName: typeName, Size: size, RepeatCount: 1}
}

func (self *DeviceField) TotalSize() int {
    return self.Size * self.RepeatCount
}

// structure expansion

func (self *ExpandedDeviceStructureDefinition) addExpandedField(f *DeviceField, pathSoFar string, direction uint) {
    for i := 0; i < f.RepeatCount; i = i + 1 {
        var pathPart string = ""
        if f.RepeatCount > 1 {
            pathPart = fmt.Sprintf("%s/%d", f.Name, i)
        } else {
            pathPart = f.Name
        }
        path := pathSoFar + "/" + pathPart
        if IsAtomic(f) {
            alignment := AlignmentOf(f.TypeName)
            var paddingRequired int
            if self.Size == 0 || self.Size%alignment == 0 {
                paddingRequired = 0
            } else {
                paddingRequired = alignment - self.Size%alignment
            }
            offset := self.Size + paddingRequired
            newField := &ExpandedField{FieldDefinition: f, Offset: offset, Size: f.Size, Path: path}
            if f.IsConstant {
                newField.Value = f.Constant
            }
            self.Fields = append(self.Fields, newField)
            self.Size = offset + f.Size
            //fmt.Printf("%s (%s)\n  size: %d\n  alignment: %d\n  padding: %d\n  offset: %d\n  total: %d\n  path: %s\n", f.Name, f.TypeName, newField.Size, alignment, paddingRequired, offset, self.Size, path)
        } else {
            s := Global.ValueOf(SymbolWithName(f.TypeName))
            st := (*DeviceStructure)(ObjectValue(s))
            var def *DeviceStructureDefinition
            if direction == outgoing {
                def = st.Outgoing
            } else {
                def = st.Incoming
            }
            self.addExpandedFields(def.Fields, path, direction)
        }
    }
}

func (self *ExpandedDeviceStructureDefinition) addExpandedFields(fields []*DeviceField, pathSoFar string, direction uint) {
    for _, f := range fields {
        self.addExpandedField(f, pathSoFar, direction)
    }
}

func (self *DeviceStructureDefinition) Expand(parent *ExpandedDeviceStructure, direction uint) *ExpandedDeviceStructureDefinition {
    newStruct := &ExpandedDeviceStructureDefinition{Parent: parent}
    newStruct.addExpandedFields(self.Fields, "", direction)
    return newStruct
}

func (self *DeviceStructure) Expand(parent *DeviceDeclaration) *ExpandedDeviceStructure {
    newStruct := &ExpandedDeviceStructure{Name: self.Name, Parent: parent}
    newStruct.Outgoing = self.Outgoing.Expand(newStruct, outgoing)
    newStruct.Incoming = self.Incoming.Expand(newStruct, incoming)
    return newStruct
}

func (self *DeviceDeclaration) Expand() {
    for _, s := range self.Structures {
        self.ExpandedStructures[s.Name] = s.Expand(self)
    }
}

// serialization into byte array

func AddFieldToByteArray(f *ExpandedField, bytes *[]byte) {
    switch f.FieldDefinition.TypeName {
    case "uint8":
        addUint8ToByteArray(uint8(f.Value), f.Offset, bytes)
    case "uint16":
        addUint16ToByteArray(uint16(f.Value), f.Offset, bytes)
    case "uint32":
        addUint32ToByteArray(uint32(f.Value), f.Offset, bytes)
    }
}

func (self *ExpandedDeviceStructureDefinition) ByteArray() *[]byte {
    bytes := make([]byte, self.Size)
    for _, f := range self.Fields {
        AddFieldToByteArray(f, &bytes)
    }
    return &bytes
}

func stepsFromPath(path string) (steps []string) {
    return strings.Split(path, "/")[1:]
}

// validation

func (self *ExpandedField) Validate(env *SymbolTableFrame) bool {
    fieldDef := self.FieldDefinition
    if fieldDef.IsConstant {
        return true
    }

    if fieldDef.ValidRange != nil {
        return fieldDef.ValidRange.Validate(self.Value)
    }

    if fieldDef.ValidValues != nil {
        return fieldDef.ValidValues.Validate(self.Value)
    }

    if fieldDef.DeferredValidationCode != nil {
        CurrentField = fieldDef
        _, err := Eval(fieldDef.DeferredValidationCode, env)
        if err != nil {
            return false
        }
        result := self.Validate(env)
        fieldDef.ValidRange = nil
        fieldDef.ValidValues = nil
        return result
    }

    return true
}

func (self *ExpandedDeviceStructureDefinition) Validate() bool {
    env := NewSymbolTableFrameBelow(Global)
    for _, field := range self.Fields {
        env.BindLocallyTo(SymbolWithName(field.FieldDefinition.Name), NumberWithValue(field.Value))
        if !field.Validate(env) {
            return false
        }
    }
    return true
}

// populating from JSON

func (self *ExpandedField) extractValueFromJsonWithStepAndParent(json *Data, steps []string, parentNode *Data) {
    if self.FieldDefinition.IsConstant {
        return
    }

    if self.FieldDefinition.FromJsonTransform != nil {
        TransformJson(self.FieldDefinition.FromJsonTransform, json, parentNode)
    }

    if len(steps) == 0 {
        self.Value = uint32(NumericValue(json))
    } else {
        step := steps[0]
        i, err := strconv.ParseInt(step, 10, 32)
        if err != nil {
            // a hash key
            value, err := Assoc(StringWithValue(step), json)
            if err == nil {
                self.extractValueFromJsonWithStepAndParent(Cdr(value), steps[1:], json)
            }
        } else {
            // an array index
            self.extractValueFromJsonWithStepAndParent(Nth(json, int(i)+1), steps[1:], json)
        }
    }
}

func (self *ExpandedField) extractValueFromJson(alist *Data) {
    self.extractValueFromJsonWithStepAndParent(alist, stepsFromPath(self.Path), nil)
}

func (self *ExpandedDeviceStructureDefinition) PopulateFromJson(jsonData *Data) {
    for _, field := range self.Fields {
        field.extractValueFromJson(jsonData)
    }
}

// generating json

func (self *ExpandedField) insertIntoJson(steps []string, root *Data) *Data {
    if len(steps) == 0 {
        return NumberWithValue(self.Value)
    } else {
        step := StringWithValue(steps[0])
        _, err := strconv.ParseInt(steps[0], 10, 32)
        if err != nil { // a hash key
            if root == nil {
                root = Alist(EmptyCons())
            }
            pair, _ := Assoc(step, root)
            if pair == nil {
                root = Acons(step, self.insertIntoJson(steps[1:], nil), root)
            } else {
                root = Acons(step, self.insertIntoJson(steps[1:], Cdr(pair)), root)
            }
        } else { // an array index
            root = Append(root, self.insertIntoJson(steps[1:], nil))
        }
        return root
    }
}

func (self *ExpandedDeviceStructureDefinition) Json() *Data {
    root := Alist(EmptyCons())
    for _, f := range self.Fields {
        steps := strings.Split(f.Path, "/")[1:]
        root = f.insertIntoJson(steps, root)
    }
    return root
}

func (self *ExpandedDeviceStructureDefinition) JsonString() string {
    alist := self.Json()
    root := LispToJson(alist)
    j, err := json.Marshal(root)
    if err == nil {
        return string(j)
    } else {
        return ""
    }
}

// populating from a byte arry

func getValueForField(f *ExpandedField, bytes *[]byte) uint32 {
    if f.FieldDefinition.IsConstant {
        return f.FieldDefinition.Constant
    }

    var b uint32 = 0
    for index, count := f.Offset, 0; count < f.Size; index, count = index+1, count+1 {
        //        fmt.Printf("index: %d, count: %d, b: %d\n", index, count, b)
        b = b | (uint32((*bytes)[index])&0xff)<<(8*uint8(count))
    }
    return b
}

func (self *ExpandedDeviceStructureDefinition) PopulateFromBytes(bytes *[]byte) {
    for _, f := range self.Fields {
        val := getValueForField(f, bytes)
        f.Value = val
    }
}

// dumping function implimentation

func (self *DeviceStructureDefinition) Dump() {
    fmt.Printf("  %s (%d bytes)\n", self.Parent.Name, self.Size)
    for _, f := range self.Fields {
        if f.RepeatCount > 1 {
            fmt.Printf("    %s [%d]%s (%d bytes)\n", f.Name, f.RepeatCount, f.TypeName, f.Size)
        } else {
            fmt.Printf("    %s %s (%d bytes)\n", f.Name, f.TypeName, f.Size)
        }
    }
    return
}

func (self *DeviceStructure) Dump() {
    fmt.Printf("Outgoing:")
    self.Outgoing.Dump()
    fmt.Printf("Incoming:")
    self.Incoming.Dump()
}

func (self *ExpandedDeviceStructureDefinition) Dump(direction uint) {
    fmt.Printf("  %s (%d bytes)\n", self.Parent.Name, self.Size)
    for _, f := range self.Fields {
        fmt.Printf("    %s %s (offset: %d, size: %d bytes) path: %s\n", f.FieldDefinition.Name, f.FieldDefinition.TypeName, f.Offset, f.Size, f.Path)
    }
    return
}

func (self *DeviceStructure) DumpExpanded() {
    exp := self.Expand(self.Parent)
    fmt.Printf("Outgoing:")
    exp.Outgoing.Dump(outgoing)
    fmt.Printf("Incoming:")
    exp.Incoming.Dump(incoming)
}

func LoadDeviceDeclaration(deviceName string) {
    deviceObj, err := ProcessFile(deviceName + ".device")
    if err != nil {
        panic(errors.New(fmt.Sprintf("Error declaring device: '%s': %s", deviceName, err)))
    }
    if deviceObj == nil || !ObjectP(deviceObj) || TypeOfObject(deviceObj) != "DeviceDeclaration" {
        panic(errors.New(fmt.Sprintf("Error declaring device: '%s': %s", deviceName, err)))
    }
    device := (*DeviceDeclaration)(ObjectValue(deviceObj))
    device.Expand()
}

func getDeviceNamed(deviceName string) *DeviceDeclaration {
    deviceObj := Global.ValueOf(SymbolWithName(deviceName))
    if deviceObj == nil || !ObjectP(deviceObj) || TypeOfObject(deviceObj) != "DeviceDeclaration" {
        return nil
    }
    return (*DeviceDeclaration)(ObjectValue(deviceObj))
}

func WriteToDevice(deviceName string, jsonString string) {
    device := getDeviceNamed(deviceName)
    if device == nil {
        panic(errors.New(fmt.Sprintf("Can't find device named '%s'.", deviceName)))
    }
    json := JsonStringToLisp(jsonString)
    apiName := StringValue(Caar(json))
    jsonData := Cdar(json)

    api := device.Apis[apiName]
    structure := device.ExpandedStructures[apiName]

    if api == nil {
        panic(errors.New(fmt.Sprintf("Can't find api for %s", apiName)))
    }
    if structure == nil {
        panic(errors.New(fmt.Sprintf("Can't find structure for %s", apiName)))
    }

    out := structure.Outgoing
    out.PopulateFromJson(jsonData)
    out.Validate()
    bytes := out.ByteArray()
    Global.BindTo(SymbolWithName("payload"), ObjectWithTypeAndValue("[]byte", unsafe.Pointer(bytes)))
    writeCmd := api.Write
    bytes = writeCmd.SerializePayload()
    err := DriverToUse.Write(device.Handle, writeCmd.Cmd, bytes, uint32(len(*bytes)))
    if err != 0 {
        panic(errors.New(fmt.Sprintf("Error writing to %d: %s.", deviceName, err)))
    }
}

func ReadFromDevice(deviceName string, jsonString string) (result string) {
    device := getDeviceNamed(deviceName)
    if device == nil {
        panic(errors.New(fmt.Sprintf("Can't find device named '%s'.", deviceName)))
    }
    json := JsonStringToLisp(jsonString)
    apiName := StringValue(Caar(json))
    jsonData := Cdar(json)

    api := device.Apis[apiName]
    structure := device.ExpandedStructures[apiName]

    if api == nil {
        panic(errors.New(fmt.Sprintf("Can't find api for %s", apiName)))
    }
    if structure == nil {
        panic(errors.New(fmt.Sprintf("Can't find structure for %s", apiName)))
    }

    out := structure.Outgoing
    out.PopulateFromJson(jsonData)
    bytes := out.ByteArray()
    Global.BindTo(SymbolWithName("payload"), ObjectWithTypeAndValue("[]byte", unsafe.Pointer(bytes)))

    readCmd := api.Read
    bytes = readCmd.SerializePayload()
    err := DriverToUse.Read(device.Handle, readCmd.Cmd, bytes, uint32(len(*bytes)))
    if err != 0 {
        panic(errors.New(fmt.Sprintf("Error reading from %d: %s.", deviceName, err)))
    }
    payload := readCmd.ExtractPayload(bytes)

    in := structure.Incoming
    in.PopulateFromBytes(payload)
    innerJson := in.JsonString()
    return fmt.Sprintf("{\"%s\":%s}", apiName, innerJson)
}

func GetDevices() {
    deviceData := DriverToUse.GetDevices()

    for _, deviceInfo := range deviceData.Devices {
        //        productId := deviceInfo.ProductId
        //        dev := device.Device.FindByProductId(int(productId))
        //        device := getDeviceNamed(dev.Name)
        device := getDeviceNamed("sensei-raw")
        if device != nil {
            device.Handle = deviceInfo.DeviceHandle
        }
    }

}
