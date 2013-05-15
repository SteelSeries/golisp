// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file tests the Json<->Lisp support
package golisp

import (
    "encoding/json"
    . "launchpad.net/gocheck"
)

type JsonLispSuite struct {
}

var _ = Suite(&JsonLispSuite{})

func (s *JsonLispSuite) TestMap(c *C) {
    jsonData := `{"map": 1}`
    b := []byte(jsonData)
    var data interface{}
    jsonErr := json.Unmarshal(b, &data)
    c.Assert(jsonErr, IsNil)

    sexpr := JsonToLisp(data)
    expected := Acons(StringWithValue("map"), NumberWithValue(1), nil)

    c.Assert(IsEqual(sexpr, expected), Equals, true)
}

func (s *JsonLispSuite) TestArray(c *C) {
    jsonData := `[1, 2, 3]`
    b := []byte(jsonData)
    var data interface{}
    jsonErr := json.Unmarshal(b, &data)
    c.Assert(jsonErr, IsNil)

    sexpr := JsonToLisp(data)
    expected := InternalMakeList(NumberWithValue(1), NumberWithValue(2), NumberWithValue(3))

    c.Assert(IsEqual(sexpr, expected), Equals, true)
}

func (s *JsonLispSuite) TestMixed(c *C) {
    jsonData := `{"map": {"f1": [47, 75], "f2": 185}, "f3": 85}`
    b := []byte(jsonData)
    var data interface{}
    jsonErr := json.Unmarshal(b, &data)
    c.Assert(jsonErr, IsNil)

    sexpr := JsonToLisp(data)
    expected := Acons(StringWithValue("map"), Acons(StringWithValue("f1"), InternalMakeList(NumberWithValue(47), NumberWithValue(75)), Acons(StringWithValue("f2"), NumberWithValue(185), nil)), Acons(StringWithValue("f3"), NumberWithValue(85), nil))

    c.Assert(IsEqual(sexpr, expected), Equals, true)
}
