// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments the driver interface layer for real

package driver

type RealDriver struct {
}

func (self RealDriver) GetDevices() *DeviceList {
    senseiRaw := DeviceInfo{VendorId: uint32(0), ProductId: uint32(0), DeviceHandle: uint32(0x01)}
    l := &DeviceList{Count: uint32(1)}
    l.Devices = append(l.Devices, senseiRaw)
    return l
}

func (self RealDriver) Write(handle uint32, command uint32, data *[]byte, dataLength uint32) (err uint32) {
    return uint32(0)
}

func (self RealDriver) Read(handle uint32, command uint32, data *[]byte, dataLength uint32) (err uint32) {
    return uint32(0)
}
