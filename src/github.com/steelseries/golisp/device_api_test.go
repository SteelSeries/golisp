// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments modular device support
// This file tests device description and serialization
package golisp

import (
    //"fmt"
    "github.com/steelseries/golisp/driver"
    . "launchpad.net/gocheck"
)

type DeviceApiSuite struct {
}

var _ = Suite(&DeviceApiSuite{})

func (s *DeviceApiSuite) SetUpSuite(c *C) {
    Global = NewSymbolTableFrameBelow(nil)
    InitBuiltins()
    InitDeviceBuiltins()
}

func (s *DeviceApiSuite) TestByteArrayWrapping(c *C) {
    data := []byte{1, 2, 3, 4}
    wrapped := WrapByteArray(42, &data)
    c.Assert(len(*wrapped), Equals, 12)

    // type tag
    c.Assert((*wrapped)[0], Equals, byte(42))
    c.Assert((*wrapped)[1], Equals, byte(0))
    c.Assert((*wrapped)[2], Equals, byte(0))
    c.Assert((*wrapped)[3], Equals, byte(0))

    // size
    c.Assert((*wrapped)[4], Equals, byte(4))
    c.Assert((*wrapped)[5], Equals, byte(0))
    c.Assert((*wrapped)[6], Equals, byte(0))
    c.Assert((*wrapped)[7], Equals, byte(0))

    // data
    c.Assert((*wrapped)[8], Equals, byte(1))
    c.Assert((*wrapped)[9], Equals, byte(2))
    c.Assert((*wrapped)[10], Equals, byte(3))
    c.Assert((*wrapped)[11], Equals, byte(4))
}

func (s *DeviceApiSuite) TestListToByteArray(c *C) {
    var a []*Data
    d := []uint32{uint32(1), uint32(2), uint32(3), uint32(4), uint32(5)}
    for _, d := range d {
        a = append(a, NumberWithValue(d))
    }
    l := ArrayToList(a)
    b := ListToByteArray(l, Global)
    c.Assert((*b)[0], Equals, byte(1))
    c.Assert((*b)[1], Equals, byte(2))
    c.Assert((*b)[2], Equals, byte(3))
    c.Assert((*b)[3], Equals, byte(4))
    c.Assert((*b)[4], Equals, byte(5))
}

func (s *DeviceApiSuite) TestDefChunk(c *C) {
    code := "(chunk 10 8 (list-to-bytearray '(1 2 3 4 5 6 7 8)))"
    sexpr, err := Parse(code)
    chunkObj, err := Eval(sexpr, Global)
    c.Assert(err, IsNil)
    c.Assert(TypeOfObject(chunkObj), Equals, "ApiChunk")
    chunk := (*ApiChunk)(ObjectValue(chunkObj))
    c.Assert(chunk.DataType, Equals, uint32(10))
    c.Assert(chunk.DataSize, Equals, uint32(8))
}

func (s *DeviceApiSuite) TestSerializingChunk(c *C) {
    code := "(chunk 10 8 (list-to-bytearray '(1 2 3 4 5 6 7 8)))"
    sexpr, err := Parse(code)
    chunkObj, err := Eval(sexpr, Global)
    c.Assert(err, IsNil)
    c.Assert(TypeOfObject(chunkObj), Equals, "ApiChunk")
    chunk := (*ApiChunk)(ObjectValue(chunkObj))
    bytes := chunk.Serialize()
    c.Assert((*bytes)[0], Equals, byte(16))
    c.Assert((*bytes)[1], Equals, byte(0))
    c.Assert((*bytes)[2], Equals, byte(0))
    c.Assert((*bytes)[3], Equals, byte(0))
    c.Assert((*bytes)[4], Equals, byte(10))
    c.Assert((*bytes)[5], Equals, byte(0))
    c.Assert((*bytes)[6], Equals, byte(0))
    c.Assert((*bytes)[7], Equals, byte(0))
    c.Assert((*bytes)[8], Equals, byte(8))
    c.Assert((*bytes)[9], Equals, byte(0))
    c.Assert((*bytes)[10], Equals, byte(0))
    c.Assert((*bytes)[11], Equals, byte(0))
    c.Assert((*bytes)[12], Equals, byte(1))
    c.Assert((*bytes)[13], Equals, byte(2))
    c.Assert((*bytes)[14], Equals, byte(3))
    c.Assert((*bytes)[15], Equals, byte(4))
    c.Assert((*bytes)[16], Equals, byte(5))
    c.Assert((*bytes)[17], Equals, byte(6))
    c.Assert((*bytes)[18], Equals, byte(7))
    c.Assert((*bytes)[19], Equals, byte(8))
}

func (s *DeviceApiSuite) TestDefRead(c *C) {
    CurrentApi = &DeviceApi{Name: "test", Env: Global}
    code := "(read 10 (chunk 10 4 (list-to-bytearray '(1 2 3 4))))"
    sexpr, err := Parse(code)
    _, err = Eval(sexpr, Global)
    c.Assert(err, IsNil)

    reader := CurrentApi.Read
    c.Assert(reader.Cmd, Equals, uint32(10))
    c.Assert(len(reader.Chunks), Equals, 1)

    c.Assert(CurrentApi.Write, IsNil)
}

func (s *DeviceApiSuite) TestDefWrite(c *C) {
    CurrentApi = &DeviceApi{Name: "test", Env: Global}
    code := "(write 10 (chunk 10 4 (list-to-bytearray '(1 2 3 4))))"
    sexpr, err := Parse(code)
    _, err = Eval(sexpr, Global)
    c.Assert(err, IsNil)

    writer := CurrentApi.Write
    c.Assert(writer.Cmd, Equals, uint32(10))
    c.Assert(len(writer.Chunks), Equals, 1)

    c.Assert(CurrentApi.Read, IsNil)
}

func (s *DeviceApiSuite) TestDefApi(c *C) {
    code := "(def-api test (read 10 (chunk 10 4 (list-to-bytearray '(1 2 3 4)))) (write 11 (chunk 10 4 (list-to-bytearray '(1 2 3 4)))))"
    sexpr, err := Parse(code)
    c.Assert(err, IsNil)
    apiObj, err := Eval(sexpr, Global)
    c.Assert(err, IsNil)
    c.Assert(apiObj, NotNil)
    api := (*DeviceApi)(ObjectValue(apiObj))
    c.Assert(api.Name, Equals, "test")

    reader := CurrentApi.Read
    c.Assert(reader.Cmd, Equals, uint32(10))
    c.Assert(len(reader.Chunks), Equals, 1)

    writer := CurrentApi.Write
    c.Assert(writer.Cmd, Equals, uint32(11))
    c.Assert(len(writer.Chunks), Equals, 1)
}

func (s *DeviceApiSuite) TestDefApiWithExtraCode(c *C) {
    code := `(def-api test
                      (define read-cmd 10)
                      (read read-cmd (chunk 10 4 (list-to-bytearray '(1 2 3 4))))
                      (let ((write-cmd 11))
                           (write write-cmd (chunk 10 4 (list-to-bytearray '(1 2 3 4))))))`
    sexpr, err := Parse(code)
    c.Assert(err, IsNil)
    apiObj, err := Eval(sexpr, Global)
    c.Assert(err, IsNil)
    c.Assert(apiObj, NotNil)
    api := (*DeviceApi)(ObjectValue(apiObj))
    c.Assert(api.Name, Equals, "test")

    reader := CurrentApi.Read
    c.Assert(reader.Cmd, Equals, uint32(10))
    c.Assert(len(reader.Chunks), Equals, 1)

    writer := CurrentApi.Write
    c.Assert(writer.Cmd, Equals, uint32(11))
    c.Assert(len(writer.Chunks), Equals, 1)
}

func (s *DeviceApiSuite) TestSerializingMultiChunkPayload(c *C) {
    CurrentApi = &DeviceApi{Name: "test", Env: Global}
    code := `(read 10 
                   (chunk 1 8 (list-to-bytearray '(1 2 3 4 5 6 7 8)))
                   (chunk 5 2 (list-to-bytearray '(9 10))))`
    sexpr, err := Parse(code)
    _, err = Eval(sexpr, Global)
    c.Assert(err, IsNil)

    reader := CurrentApi.Read
    bytes := reader.SerializePayload()

    c.Assert(len(*bytes), Equals, 36)

    // chunk 1 is 16 bytes
    c.Assert((*bytes)[0], Equals, byte(16))
    c.Assert((*bytes)[1], Equals, byte(0))
    c.Assert((*bytes)[2], Equals, byte(0))
    c.Assert((*bytes)[3], Equals, byte(0))
    // chunk 1 has type 1
    c.Assert((*bytes)[4], Equals, byte(1))
    c.Assert((*bytes)[5], Equals, byte(0))
    c.Assert((*bytes)[6], Equals, byte(0))
    c.Assert((*bytes)[7], Equals, byte(0))
    // chunk 1 data is 8 bytes
    c.Assert((*bytes)[8], Equals, byte(8))
    c.Assert((*bytes)[9], Equals, byte(0))
    c.Assert((*bytes)[10], Equals, byte(0))
    c.Assert((*bytes)[11], Equals, byte(0))
    // chunk 1 data
    c.Assert((*bytes)[12], Equals, byte(1))
    c.Assert((*bytes)[13], Equals, byte(2))
    c.Assert((*bytes)[14], Equals, byte(3))
    c.Assert((*bytes)[15], Equals, byte(4))
    c.Assert((*bytes)[16], Equals, byte(5))
    c.Assert((*bytes)[17], Equals, byte(6))
    c.Assert((*bytes)[18], Equals, byte(7))
    c.Assert((*bytes)[19], Equals, byte(8))

    // chunk 2 is 12 bytes
    c.Assert((*bytes)[20], Equals, byte(12))
    c.Assert((*bytes)[21], Equals, byte(0))
    c.Assert((*bytes)[22], Equals, byte(0))
    c.Assert((*bytes)[23], Equals, byte(0))
    // chunk 2 has type 5
    c.Assert((*bytes)[24], Equals, byte(5))
    c.Assert((*bytes)[25], Equals, byte(0))
    c.Assert((*bytes)[26], Equals, byte(0))
    c.Assert((*bytes)[27], Equals, byte(0))
    // chunk 2 is 2 bytes
    c.Assert((*bytes)[28], Equals, byte(2))
    c.Assert((*bytes)[29], Equals, byte(0))
    c.Assert((*bytes)[30], Equals, byte(0))
    c.Assert((*bytes)[31], Equals, byte(0))
    // chunk 2 data
    c.Assert((*bytes)[32], Equals, byte(9))
    c.Assert((*bytes)[33], Equals, byte(10))
    // 2 bytes of padding
    c.Assert((*bytes)[34], Equals, byte(0))
    c.Assert((*bytes)[35], Equals, byte(0))

}

func (s *DeviceApiSuite) TestWriting(c *C) {
    testDriver := driver.StubDriver{}
    DriverToUse = testDriver
    driver.TestDriver = &testDriver
    CurrentApi = &DeviceApi{Name: "test", Env: Global}
    code := `(def-device sensei-raw 
               (def-struct test 
			     (common (def-field f uint8 (repeat 4))))
			   (def-api test
 			     (write HID (chunk 0 4 payload))))`
    sexpr, err := Parse(code)
    _, err = Eval(sexpr, Global)
    c.Assert(err, IsNil)
    GetDevices()

    json := `{"test": {"f": [1, 2, 3, 4]}}`

    bytes := []byte{12, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 1, 2, 3, 4}
    testDriver.ExpectWrite(c, uint32(1), uint32(HID_PROTOCOL), &bytes, uint32(16), uint32(0))
    WriteToDevice("sensei-raw", json)
    c.Assert(testDriver.WasSatisfied(), Equals, true)
}
