// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments modular device support
// This file implements device api support

package golisp

type DeviceApi struct {
}

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

func ListToByteArray(list *Data, env *SymbolTableFrame) (result []byte) {
    result = make([]byte, int(Length(list)))
    for c := list; NotNilP(c); c = Cdr(c) {
    }
    return
}
