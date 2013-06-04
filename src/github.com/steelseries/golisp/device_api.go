// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments modular device support
// This file implements device api support

package golisp

import (
    "errors"
    "fmt"
    "unsafe"
)

type DeviceApi struct {
    Name  string
    Env   *SymbolTableFrame
    Read  *ApiCommand
    Write *ApiCommand
}

type ApiCommand struct {
    Cmd    uint32
    Chunks []*ApiChunk
}

type ApiChunk struct {
    DataType uint32
    DataSize uint32
    Code     *Data
    Env      *SymbolTableFrame
}

var CurrentApi *DeviceApi

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

func WrapByteArray(typeTag uint32, data *[]byte) (wrappedByteArray *[]byte) {
    bytes := make([]byte, len(*data)+8)
    addUint32ToByteArray(typeTag, 0, &bytes)
    addUint32ToByteArray(uint32(len(*data)), 4, &bytes)
    copy(bytes[8:], *data)
    return &bytes
}

func ListToByteArray(list *Data, env *SymbolTableFrame) (result *[]byte) {
    var a []byte
    a = make([]byte, 0, int(Length(list)))
    for c := list; NotNilP(c); c = Cdr(c) {
        a = append(a, byte(NumericValue(Car(c))))
    }
    return &a
}

func DefApi(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    name := StringValue(Car(args))
    apiEnv := NewSymbolTableFrameBelow(env)
    CurrentApi = &DeviceApi{Name: name, Env: apiEnv}

    for c := Cdr(args); NotNilP(c); c = Cdr(c) {
        Eval(Car(c), apiEnv) // API commands have the side effect of adding themselves to the current API object
    }

    return ObjectWithTypeAndValue("DeviceApi", unsafe.Pointer(CurrentApi)), nil
}

func makeApiCommand(args *Data, env *SymbolTableFrame) (result *ApiCommand, err error) {
    cmd, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    cmdValue := NumericValue(cmd)

    chunks := make([]*ApiChunk, 0, 1)
    var obj *Data

    for c := Cdr(args); NotNilP(c); c = Cdr(c) {
        obj, err = Eval(Car(c), env)
        if !ObjectP(obj) || TypeOfObject(obj) != "ApiChunk" {
            err = errors.New(fmt.Sprintf("API commands expect a series of chunks but a %s was found.", TypeOfObject(obj)))
            return
        }

        chunks = append(chunks, (*ApiChunk)(ObjectValue(obj)))
    }

    return &ApiCommand{Cmd: cmdValue, Chunks: chunks}, nil
}

func ApiRead(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    CurrentApi.Read, err = makeApiCommand(args, env)
    if err != nil {
        return
    }
    return
}

func ApiWrite(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    CurrentApi.Write, err = makeApiCommand(args, env)
    if err != nil {
        return
    }
    return
}

func DefChunk(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    var val *Data
    val, err = Eval(Car(args), env)
    if err != nil {
        return
    }
    typeVal := NumericValue(val)

    val, err = Eval(Cadr(args), env)
    if err != nil {
        return
    }
    sizeVal := NumericValue(val)

    chunk := &ApiChunk{DataType: typeVal, DataSize: sizeVal, Code: Caddr(args), Env: env}
    result = ObjectWithTypeAndValue("ApiChunk", unsafe.Pointer(chunk))
    return
}

func (self *ApiChunk) Serialize() (result *[]byte) {
    bytesNeeded := ((self.DataSize + 3) / 4) * 4
    paddingNeeded := bytesNeeded - self.DataSize
    chunkSize := bytesNeeded + 8

    bytes := make([]byte, chunkSize+4)
    addUint32ToByteArray(chunkSize, 0, &bytes)
    addUint32ToByteArray(self.DataType, 4, &bytes)
    addUint32ToByteArray(self.DataSize, 8, &bytes)

    dataByteObject, err := Eval(self.Code, self.Env)
    if err != nil {
        panic(err)
    }
    if !ObjectP(dataByteObject) || TypeOfObject(dataByteObject) != "[]byte" {
        panic(errors.New(fmt.Sprintf("API chunk code should return []byte but returned %s.", TypeOfObject(dataByteObject))))
    }

    dataBytes := (*[]byte)(ObjectValue(dataByteObject))

    if uint32(len(*dataBytes)) > self.DataSize {
        panic(errors.New(fmt.Sprintf("Expect no more than %d bytes of data, but got %d.", self.DataSize, len(*dataBytes))))
    }

    extraPadding := self.DataSize - uint32(len(*dataBytes))
    copy(bytes[12:], *dataBytes)
    for i, offset := paddingNeeded+extraPadding, int(self.DataSize)+12; i > 0; i, offset = i-1, offset+1 {
        addUint8ToByteArray(0, offset, &bytes)
    }
    return &bytes
}

func (self *ApiCommand) SerializePayload() (result *[]byte) {
    chunks := make([]*[]byte, 0, len(self.Chunks))
    total := 0
    for _, chunk := range self.Chunks {
        newChunk := chunk.Serialize()
        chunks = append(chunks, newChunk)
        total += len(*newChunk)
    }
    payload := make([]byte, total)
    offset := 0
    for _, chunk := range chunks {
        offset += copy(payload[offset:], *chunk)
    }
    return &payload
}

func getUint32(bytes *[]byte, offset int) uint32 {
    var acc uint32
    acc = uint32((*bytes)[offset])
    acc += uint32((*bytes)[offset+1]) << 8
    acc += uint32((*bytes)[offset+2]) << 16
    acc += uint32((*bytes)[offset+3]) << 24
    return acc
}

func (self *ApiCommand) ExtractPayload(bytes *[]byte) (result *[]byte) {
    var offset = 0
    var size = 0
    var tag = 0
    for offset < len(*bytes) {
        size = int(getUint32(bytes, offset))
        tag = int(getUint32(bytes, offset+4))
        if tag == 0 {
            dataSize := int(getUint32(bytes, offset+8))
            byteArray := make([]byte, dataSize)
            copy(byteArray, (*bytes)[offset+12:offset+12+dataSize])
            return &byteArray
        }
        offset += size + 4
    }
    panic(errors.New("payload data chunk not found"))
}
