// Copyright 2013 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file tests the Json<->Lisp support
package golisp

import (
    . "launchpad.net/gocheck"
)

type JsonLispSuite struct {
}

var _ = Suite(&JsonLispSuite{})

func (s *JsonLispSuite) TestJsonToLispMap(c *C) {
    jsonData := `{"map": 1}`
    sexpr := JsonStringToLisp(jsonData)
    expected := Acons(StringWithValue("map"), NumberWithValue(1), nil)

    c.Assert(IsEqual(sexpr, expected), Equals, true)
}

func (s *JsonLispSuite) TestJsonToLispArray(c *C) {
    jsonData := `[1, 2, "hi"]`
    sexpr := JsonStringToLisp(jsonData)
    expected := InternalMakeList(NumberWithValue(1), NumberWithValue(2), StringWithValue("hi"))

    c.Assert(IsEqual(sexpr, expected), Equals, true)
}

func (s *JsonLispSuite) TestJsonToLispMixed(c *C) {
    jsonData := `{"map": {"f1": [47, 75], "f2": 185}, "f3": 85}`
    sexpr := JsonStringToLisp(jsonData)

    expected := Acons(StringWithValue("map"),
        Acons(StringWithValue("f1"),
            InternalMakeList(NumberWithValue(47), NumberWithValue(75)),
            Acons(StringWithValue("f2"),
                NumberWithValue(185), nil)),
        Acons(StringWithValue("f3"),
            NumberWithValue(85), nil))

    c.Assert(IsEqual(sexpr, expected), Equals, true)
}

func (s *JsonLispSuite) TestLispToJsonMap(c *C) {
    alist := Acons(StringWithValue("map"), NumberWithValue(1), nil)
    data := LispToJsonString(alist)
    c.Assert(data, Equals, `{"map":1}`)
}

func (s *JsonLispSuite) TestLispToJsonArray(c *C) {
    alist := InternalMakeList(NumberWithValue(1), NumberWithValue(2), StringWithValue("hi"))
    data := LispToJsonString(alist)
    c.Assert(data, Equals, `[1,2,"hi"]`)
}

func (s *JsonLispSuite) TestLispToJsonMixed(c *C) {
    alist := Acons(StringWithValue("map"), Acons(StringWithValue("f1"), InternalMakeList(NumberWithValue(47), NumberWithValue(75)), Acons(StringWithValue("f2"), NumberWithValue(185), nil)), Acons(StringWithValue("f3"), NumberWithValue(85), nil))
    data := LispToJsonString(alist)
    c.Assert(data, Equals, `{"f3":85,"map":{"f1":[47,75],"f2":185}}`)
}

func (s *JsonLispSuite) TestSimpleJsonTransformation(c *C) {
    jsonData := Acons(StringWithValue("map"), Acons(StringWithValue("f1"), InternalMakeList(NumberWithValue(47), NumberWithValue(75)), Acons(StringWithValue("f2"), NumberWithValue(185), nil)), Acons(StringWithValue("f3"), NumberWithValue(85), nil))

    xform, err := Parse(`(lambda (node parent) (acons "f3" 42 parent))`)
    c.Assert(err, IsNil)
    parent := jsonData
    pair, _ := Assoc(StringWithValue("f3"), jsonData)
    _, err = TransformJson(xform, Cdr(pair), parent)
    c.Assert(err, IsNil)

    var newNode *Data
    newNode, err = Assoc(StringWithValue("f3"), jsonData)
    c.Assert(err, IsNil)
    c.Assert(NumericValue(Cdr(newNode)), Equals, uint32(42))
}

func (s *JsonLispSuite) TestSimpleJsonTransformationReturnsNewValue(c *C) {
    jsonData := Acons(StringWithValue("map"), Acons(StringWithValue("f1"), InternalMakeList(NumberWithValue(47), NumberWithValue(75)), Acons(StringWithValue("f2"), NumberWithValue(185), nil)), Acons(StringWithValue("f3"), NumberWithValue(85), nil))

    xform, err := Parse(`(lambda (node parent) (+ node 5))`)
    c.Assert(err, IsNil)
    parent := jsonData
    pair, _ := Assoc(StringWithValue("f3"), jsonData)
    newValue, err := TransformJson(xform, Cdr(pair), parent)
    c.Assert(err, IsNil)

    c.Assert(NumericValue(newValue), Equals, uint32(90))
}

func (s *JsonLispSuite) TestMoreComplexJsonTransformation(c *C) {
    jsonData := Acons(StringWithValue("map"), Acons(StringWithValue("f1"), InternalMakeList(NumberWithValue(47), NumberWithValue(75)), Acons(StringWithValue("f2"), NumberWithValue(185), nil)), Acons(StringWithValue("f3"), NumberWithValue(85), nil))

    xform, err := Parse(`(lambda (node parent) (acons "f3" '(1 2 3) parent))`)
    c.Assert(err, IsNil)
    parent := jsonData
    pair, _ := Assoc(StringWithValue("f3"), jsonData)
    _, err = TransformJson(xform, Cdr(pair), parent)
    c.Assert(err, IsNil)

    var newNode *Data
    newNode, err = Assoc(StringWithValue("f3"), jsonData)
    c.Assert(err, IsNil)
    c.Assert(IsEqual(Cdr(newNode), InternalMakeList(NumberWithValue(1), NumberWithValue(2), NumberWithValue(3))), Equals, true)
}

func (s *JsonLispSuite) TestEvenMoreComplexJsonTransformation(c *C) {
    jsonData := Acons(StringWithValue("map"), Acons(StringWithValue("f1"), InternalMakeList(NumberWithValue(47), NumberWithValue(75)), Acons(StringWithValue("f2"), NumberWithValue(185), nil)), Acons(StringWithValue("f3"), NumberWithValue(85), nil))

    xform, err := Parse(`(lambda (node parent) (acons "f3" (acons "a" (+ node 1) nil) parent))`)
    c.Assert(err, IsNil)
    parent := jsonData
    pair, _ := Assoc(StringWithValue("f3"), jsonData)
    _, err = TransformJson(xform, Cdr(pair), parent)
    c.Assert(err, IsNil)

    var newNode *Data
    var newerNode *Data
    newNode, err = Assoc(StringWithValue("f3"), jsonData)
    c.Assert(err, IsNil)
    newerNode, err = Assoc(StringWithValue("a"), Cdr(newNode))
    c.Assert(err, IsNil)
    c.Assert(NumericValue(Cdr(newerNode)), Equals, uint32(86))
}
