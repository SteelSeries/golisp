// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments the driver interface layer for testing

package driver

import (
    "fmt"
    . "launchpad.net/gocheck"
    "strings"
)

type StubDriver struct {
    ShouldRead  bool
    ShouldWrite bool
    GotIt       bool
    Handle      uint32
    Protocol    uint32
    ToReturn    *[]byte
    Data        *[]byte
    DataLength  uint32
    Err         uint32
    CC          *C
}

var TestDriver *StubDriver

func (self StubDriver) ExpectWrite(c *C, handle uint32, protocol uint32, data *[]byte, dataLength uint32, err uint32) {
    TestDriver.CC = c
    TestDriver.ShouldRead = false
    TestDriver.ShouldWrite = true
    TestDriver.Handle = handle
    TestDriver.Protocol = protocol
    TestDriver.Data = data
    TestDriver.DataLength = dataLength
    TestDriver.Err = err
    TestDriver.GotIt = false
}

func (self StubDriver) ExpectRead(c *C, handle uint32, protocol uint32, data *[]byte, toReturn *[]byte, dataLength uint32, err uint32) {
    TestDriver.CC = c
    TestDriver.ShouldRead = true
    TestDriver.ShouldWrite = false
    TestDriver.Handle = handle
    TestDriver.Protocol = protocol
    TestDriver.ToReturn = toReturn
    TestDriver.Data = data
    TestDriver.DataLength = dataLength
    TestDriver.Err = err
    TestDriver.GotIt = false
}

func (self StubDriver) WasSatisfied() bool {
    return TestDriver.GotIt
}

func (self StubDriver) GetDevices() *DeviceList {
    senseiRaw := DeviceInfo{VendorId: uint32(0x1038), ProductId: uint32(0x1362), DeviceHandle: uint32(0x01)}
    l := &DeviceList{Count: uint32(1), Devices: make([]DeviceInfo, 1)}
    l.Devices = append(l.Devices, senseiRaw)
    return l
}

func bytearryToString(bytes *[]byte) string {
    s := make([]string, 0, len(*bytes))
    for _, b := range *bytes {
        s = append(s, fmt.Sprintf("%d", b))
    }
    return fmt.Sprintf("[%s]", strings.Join(s, ", "))
}

func printBytearrays(one *[]byte, two *[]byte) {
    fmt.Printf("Got: %s\n", bytearryToString(one))
    fmt.Printf("Exp: %s\n", bytearryToString(two))
}

func compareBytearrays(one *[]byte, two *[]byte) bool {
    if len(*one) != len(*two) {
        printBytearrays(one, two)
        return false
    }
    for i, b := range *one {
        if b != (*two)[i] {
            printBytearrays(one, two)
            return false
        }
    }
    return true
}

func (self StubDriver) Write(handle uint32, protocol uint32, data *[]byte, dataLength uint32) (err uint32) {
    TestDriver.CC.Assert(TestDriver.ShouldWrite, Equals, true)
    TestDriver.CC.Assert(handle, Equals, TestDriver.Handle)
    TestDriver.CC.Assert(protocol, Equals, TestDriver.Protocol)
    TestDriver.CC.Assert(compareBytearrays(data, TestDriver.Data), Equals, true)
    TestDriver.CC.Assert(dataLength, Equals, TestDriver.DataLength)
    TestDriver.GotIt = true
    return TestDriver.Err
}

func (self StubDriver) Read(handle uint32, protocol uint32, data *[]byte, dataLength uint32) (err uint32) {
    TestDriver.CC.Assert(TestDriver.ShouldRead, Equals, true)
    TestDriver.CC.Assert(handle, Equals, TestDriver.Handle)
    TestDriver.CC.Assert(protocol, Equals, TestDriver.Protocol)
    TestDriver.CC.Assert(compareBytearrays(data, TestDriver.Data), Equals, true)
    TestDriver.CC.Assert(dataLength, Equals, TestDriver.DataLength)
    TestDriver.GotIt = true
    (*data)[8] = uint8(len(*TestDriver.ToReturn))
    copy((*data)[12:], *TestDriver.ToReturn)
    return TestDriver.Err
}
