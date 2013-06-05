// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments the driver interface layer for testing

package driver

import (
    . "launchpad.net/gocheck"
)

type StubDriver struct {
    ShouldRead  bool
    ShouldWrite bool
    GotIt       bool
    Handle      uint32
    Command     uint32
    Data        *[]byte
    DataLength  uint32
    Err         uint32
    CC          *C
}

func (self StubDriver) ExpectWrite(c *C, handle uint32, command uint32, data *[]byte, dataLength uint32, err uint32) {
    self.CC = c
    self.ShouldRead = false
    self.ShouldWrite = true
    self.Handle = handle
    self.Command = command
    self.Data = data
    self.DataLength = dataLength
    self.Err = err
    self.GotIt = false
}

func (self StubDriver) ExpectRead(c *C, handle uint32, command uint32, data *[]byte, dataLength uint32, err uint32) {
    self.CC = c
    self.ShouldRead = true
    self.ShouldWrite = false
    self.Handle = handle
    self.Command = command
    self.Data = data
    self.DataLength = dataLength
    self.Err = err
    self.GotIt = false
}

func (self StubDriver) WasSatisfied() bool {
    return self.GotIt
}

func (self StubDriver) GetDevices() *DeviceList {
    senseiRaw := DeviceInfo{VendorId: uint32(0x1038), ProductId: uint32(0x1362), DeviceHandle: uint32(0x01)}
    l := &DeviceList{Count: uint32(1)}
    l.Devices = append(l.Devices, senseiRaw)
    return l
}

func (self StubDriver) Write(handle uint32, command uint32, data *[]byte, dataLength uint32) (err uint32) {
    if !self.ShouldWrite {
        self.CC.Errorf("Unexpected device API write call: %d, %d", handle, command)
    }
    self.CC.Assert(handle, Equals, self.Handle)
    self.CC.Assert(command, Equals, self.Command)
    self.CC.Assert(*data, Equals, *self.Data)
    self.CC.Assert(dataLength, Equals, self.DataLength)
    self.GotIt = true
    return self.Err
}

func (self StubDriver) Read(handle uint32, command uint32, data *[]byte, dataLength uint32) (err uint32) {
    if !self.ShouldRead {
        self.CC.Errorf("Unexpected device API read call: %d, %d", handle, command)
    }
    self.CC.Assert(handle, Equals, self.Handle)
    self.CC.Assert(command, Equals, self.Command)
    self.CC.Assert(*data, Equals, *self.Data)
    self.CC.Assert(dataLength, Equals, self.DataLength)
    self.GotIt = true
    return self.Err
}
