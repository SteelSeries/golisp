// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments the driver interface layer
// This file is the interface that driver api layers must conform to

package driver

type DeviceInfo struct {
    VendorId     uint32
    ProductId    uint32
    DeviceHandle uint32
}

type DeviceList struct {
    Count   uint32
    Devices []DeviceInfo
}

type Driver interface {
    GetDevices() *DeviceList
    Write(handle uint32, command uint32, data *[]byte, dataLength uint32) (err uint32)
    Read(handle uint32, command uint32, data *[]byte, dataLength uint32) (err uint32)
}
