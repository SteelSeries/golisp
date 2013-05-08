package golisp

import (
    //    "reflect"
    "encoding/json"
    "errors"
    "fmt"
    "strconv"
    "strings"
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
    if !ObjectP(typeValue) || typeValue.ObjType != "DeviceStructure" {
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
    Path            string
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

func (self *ExpandedStructure) addExpandedField(f *DeviceField, pathSoFar string) {
    for i := 0; i < f.Count; i = i + 1 {
        var pathPart string = ""
        if f.Count > 1 {
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
            self.Fields = append(self.Fields, newField)
            self.Size = offset + f.Size
            //            fmt.Printf("%s (%s)\n  size: %d\n  alignment: %d\n  padding: %d\n  offset: %d\n  total: %d\n  path: %s\n", f.Name, f.TypeName, newField.Size, alignment, paddingRequired, offset, self.Size, path)
        } else {
            s := ValueOf(SymbolWithName(f.TypeName))
            self.addExpandedFields((*DeviceStructure)(ObjectValue(s)).Fields, path)
        }
    }
}

func (self *ExpandedStructure) addExpandedFields(fields []*DeviceField, pathSoFar string) {
    for _, f := range fields {
        self.addExpandedField(f, pathSoFar)
    }
}

func (self *DeviceStructure) Expand() *ExpandedStructure {
    newStruct := &ExpandedStructure{Name: self.Name}
    newStruct.addExpandedFields(self.Fields, "")
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

// populating from JSON

func ExtractFromJsonWithStep(json interface{}, steps []string) uint32 {
    //    fmt.Printf("%v\n", steps)
    if len(steps) == 0 {
        return uint32(json.(float64))
    } else {
        step := steps[0]
        i, err := strconv.ParseInt(step, 10, 32)
        if err != nil {
            // a hash key
            return ExtractFromJsonWithStep((json.(map[string]interface{}))[step], steps[1:])
        } else {
            // an array index
            return ExtractFromJsonWithStep((json.([]interface{}))[i], steps[1:])
        }
    }
}

func ExtractFromJson(json interface{}, path string) uint32 {
    steps := strings.Split(path, "/")[1:]
    return ExtractFromJsonWithStep(json, steps)
}

func (self *ExpandedStructure) PopulateFromJson(jsonData string) {
    b := []byte(jsonData)
    var data interface{}
    err := json.Unmarshal(b, &data)
    if err != nil {
        panic(errors.New("Badly formed json"))
    }
    for _, field := range self.Fields {
        field.Value = ExtractFromJson(data, field.Path)
    }
}

// generating json

func (self *ExpandedField) insertIntoJson(steps []string, root interface{}) interface{} {
    if len(steps) == 0 {
        //        fmt.Printf("returning value: %d\n", self.Value)
        return self.Value
    } else {
        step := steps[0]
        //        fmt.Printf("formatting: %s\n", step)

        _, err := strconv.ParseInt(step, 10, 32)
        if err != nil { // a hash key
            if root == nil {
                root = make(map[string]interface{})
            }
            v, ok := (root.(map[string]interface{}))[step]
            if !ok {
                (root.(map[string]interface{}))[step] = self.insertIntoJson(steps[1:], nil)
            } else {
                (root.(map[string]interface{}))[step] = self.insertIntoJson(steps[1:], v)
            }
        } else { // an array index
            if root == nil {
                root = make([]interface{}, 0, 5)
            }
            //            fmt.Printf("appending value\n")
            root = append(root.([]interface{}), self.insertIntoJson(steps[1:], nil))
            //            fmt.Printf("array now: %v\n", root.([]interface{}))
        }
        //        fmt.Printf("Returning %v\n", root)
        return root
    }
}

func (self *ExpandedStructure) Json() interface{} {
    root := make(map[string]interface{})
    for _, f := range self.Fields {
        steps := strings.Split(f.Path, "/")[1:]
        f.insertIntoJson(steps, root)
    }
    return root
}

func (self *ExpandedStructure) JsonString() string {
    root := self.Json()
    j, err := json.Marshal(root)
    if err == nil {
        return string(j)
    } else {
        return ""
    }
}

// populating from a byte arry

func getValueForField(f *ExpandedField, bytes *[]byte) uint32 {
    var b uint32 = 0
    for index, count := f.Offset, 0; count < f.Size; index, count = index+1, count+1 {
        //        fmt.Printf("index: %d, count: %d, b: %d\n", index, count, b)
        b = b | (uint32((*bytes)[index])&0xff)<<(8*uint8(count))
    }
    return b
}

func (self *ExpandedStructure) PopulateFromBytes(bytes *[]byte) {
    for _, f := range self.Fields {
        val := getValueForField(f, bytes)
        f.Value = val
    }
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
        fmt.Printf("  %s %s (offset: %d, size: %d bytes) path: %s\n", f.FieldDefinition.Name, f.FieldDefinition.TypeName, f.Offset, f.Size, f.Path)
    }
    return
}
