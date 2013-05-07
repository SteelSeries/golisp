package golisp

import (
    //    "reflect"
    "fmt"
)

type DeviceStructure struct {
    Name   string
    Fields []*DeviceField
    Size   int // size of the struct, in buyes
}

type DeviceField struct {
    Name              string
    TypeName          string
    Size              int // size of a single element, in bytes
    Count             int // number of elements
    ToJsonTransform   *Data
    FromJsonTransform *Data
}

// helper functions

func IsValidType(typeName *Data) bool {
    typeNameString := StringValue(typeName)
    if typeNameString == "uint8" || typeNameString == "uint16" || typeNameString == "uint32" {
        return true
    }

    typeValue := ValueOf(typeName)
    if !ObjectP(typeValue) {
        return false
    }

    if typeValue.ObjType != "DeviceStructure" {
        return false
    }

    fieldType := (*DeviceStructure)(ObjectValue(typeValue))
    return fieldType.Name == typeNameString
}

func FieldSizeOf(typeName *Data) int {
    switch StringValue(typeName) {
    case "uint8":
        return 1
    case "uint16":
        return 2
    case "uint32":
        return 4
    default:
        {
            typeValue := ValueOf(typeName)
            fieldType := (*DeviceStructure)(ObjectValue(typeValue))
            return fieldType.SizeOf()
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

type ExpandedStructure struct {
    Name   string
    Fields []*ExpandedField
    Size   int // size of the struct, in bytes
}

type ExpandedField struct {
    FieldDefinition *DeviceField
    Offset          int
    Value           uint32
    Size            int
}

// DeviceStructure functions

func NewStruct(n string) (s *DeviceStructure) {
    return &DeviceStructure{Name: n, Fields: make([]*DeviceField, 0, 5), Size: 0}
}

func (self *DeviceStructure) AddField(f *DeviceField) {
    self.Fields = append(self.Fields, f)
    self.Size += f.TotalSize()
}

func (self *DeviceStructure) SizeOf() int {
    return self.Size
}

// DeviceField functions

func NewField(name string, typeName string, size int, count int, toJsonXform *Data, fromJsonXform *Data) (f *DeviceField) {
    return &DeviceField{Name: name, TypeName: typeName, Size: size, Count: count, ToJsonTransform: toJsonXform, FromJsonTransform: fromJsonXform}
}

func (self *DeviceField) TotalSize() int {
    return self.Size * self.Count
}

// structure expansion

func (self *ExpandedStructure) addExpandedField(f *DeviceField) {
    for i := f.Count; i > 0; i = i - 1 {
        if IsAtomic(f) {
            alignment := AlignmentOf(f.TypeName)
            var paddingRequired int
            if self.Size == 0 {
                paddingRequired = 0
            } else if self.Size%alignment == 0 {
                paddingRequired = 0
            } else {
                paddingRequired = alignment - self.Size%alignment
            }
            offset := self.Size + paddingRequired
            newField := &ExpandedField{FieldDefinition: f, Offset: offset, Size: f.Size}
            self.Fields = append(self.Fields, newField)
            self.Size = offset + f.Size
            //fmt.Printf("%s (%s)\n  size: %d\n  alignment: %d\n  padding: %d\n  offset: %d\n  total: %d\n\n", f.Name, f.TypeName, f.Size, alignment, paddingRequired, offset, self.Size)
        } else {
            s := ValueOf(SymbolWithName(f.TypeName))
            self.addExpandedFields((*DeviceStructure)(ObjectValue(s)).Fields)
        }
    }
}

func (self *ExpandedStructure) addExpandedFields(fields []*DeviceField) {
    for _, f := range fields {
        self.addExpandedField(f)
    }
}

func (self *DeviceStructure) Expand() *ExpandedStructure {
    newStruct := &ExpandedStructure{Name: self.Name}
    newStruct.addExpandedFields(self.Fields)
    return newStruct
}

// serialization into byte array

func addUint8ToByteArray(val uint8, offset int, bytes *[]byte) {
    (*bytes)[offset] = val
}

func addUint16ToByteArray(val uint16, offset int, bytes *[]byte) {
    (*bytes)[offset] = byte(val & 0xff)
    (*bytes)[offset+1] = byte((val >> 8) & 0xff)
}

func addUint32ToByteArray(val uint32, offset int, bytes *[]byte) {
    (*bytes)[offset] = byte(val & 0xff)
    (*bytes)[offset+1] = byte((val >> 8) & 0xff)
    (*bytes)[offset+2] = byte((val >> 16) & 0xff)
    (*bytes)[offset+3] = byte((val >> 24) & 0xff)
}

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

func (self *ExpandedStructure) ByteArray() *[]byte {
    bytes := make([]byte, self.Size)
    for _, f := range self.Fields {
        AddFieldToByteArray(f, &bytes)
    }
    return &bytes
}

// dumping function implimentation

func (self *DeviceStructure) Dump() {
    fmt.Printf("%s (%d bytes)\n", self.Name, self.Size)
    for _, f := range self.Fields {
        if f.Count > 1 {
            fmt.Printf("  %s [%d]%s (%d bytes)\n", f.Name, f.Count, f.TypeName, f.Size)
        } else {
            fmt.Printf("  %s %s (%d bytes)\n", f.Name, f.TypeName, f.Size)
        }
    }
    return
}

func (self *DeviceStructure) DumpExpanded() {
    expanded := self.Expand()
    fmt.Printf("%s (%d bytes)\n", expanded.Name, expanded.Size)
    for _, f := range expanded.Fields {
        fmt.Printf("  %s %s (offset: %d, size: %d bytes)\n", f.FieldDefinition.Name, f.FieldDefinition.TypeName, f.Offset, f.Size)
    }
    return
}
