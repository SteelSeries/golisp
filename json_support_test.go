// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file tests the Json<->Lisp support.

package golisp

import (
	. "gopkg.in/check.v1"
)

type JsonLispSuite struct {
	OldVectorizationFlag bool
}

var _ = Suite(&JsonLispSuite{})

func (s *JsonLispSuite) SetUpSuite(c *C) {
	s.OldVectorizationFlag = UseVectorization
	UseVectorization = true
}

func (s *JsonLispSuite) TearDownSuite(c *C) {
	UseVectorization = s.OldVectorizationFlag
}

func (s *JsonLispSuite) TestJsonToLispMap(c *C) {
	jsonData := `{"map": 1}`
	sexpr := JsonStringToLisp(jsonData)
	expected, _ := ParseAndEval("{map: 1}")

	c.Assert(IsEqual(sexpr, expected), Equals, true)
}

func (s *JsonLispSuite) TestJsonToLispMapWithFrames(c *C) {
	jsonData := `{"map": 1}`
	sexpr := JsonStringToLispWithFrames(jsonData)
	expected, _ := ParseAndEval("{map: 1}")

	c.Assert(IsEqual(sexpr, expected), Equals, true)
}

func (s *JsonLispSuite) TestJsonToLispArray(c *C) {
	jsonData := `[1, 2, "hi"]`
	sexpr := JsonStringToLisp(jsonData)
	expected := InternalMakeList(IntegerWithValue(1), IntegerWithValue(2), StringWithValue("hi"))

	c.Assert(IsEqual(sexpr, expected), Equals, true)
}

func (s *JsonLispSuite) TestJsonToLispBool(c *C) {
	jsonData := `{"map": true}`
	sexpr := JsonStringToLisp(jsonData)
	expected, _ := ParseAndEval("{map: #t}")

	c.Assert(IsEqual(sexpr, expected), Equals, true)
}

func (s *JsonLispSuite) TestJsonToLispMixed(c *C) {
	jsonData := `{"map": {"f1": [47, 75], "f2": 185}, "f3": 85}`
	sexpr := JsonStringToLisp(jsonData)

	expected, _ := ParseAndEval("{map: {f1: '(47 75) f2: 185} f3: 85}")

	c.Assert(IsEqual(sexpr, expected), Equals, true)
}

func (s *JsonLispSuite) TestJsonToLispMixedWithFrames(c *C) {
	jsonData := `{"map": {"f1": [47, 75], "f2": 185}, "f3": 85, "f4": 2.2}`
	sexpr := JsonStringToLispWithFrames(jsonData)
	expected, _ := ParseAndEval("{map: {f1: '(47 75) f2: 185} f3: 85 f4: 2.2}")
	c.Assert(IsEqual(sexpr, expected), Equals, true)
}

func (s *JsonLispSuite) TestLispToJsonMap(c *C) {
	alist, _ := ParseAndEval("{map: 1}")
	data := LispToJsonString(alist)
	c.Assert(data, Equals, `{"map":1}`)
}

func (s *JsonLispSuite) TestLispToJsonArray(c *C) {
	alist := InternalMakeList(IntegerWithValue(1), IntegerWithValue(2), StringWithValue("hi"))
	data := LispToJsonString(alist)
	c.Assert(data, Equals, `[1,2,"hi"]`)
}

func (s *JsonLispSuite) TestLispToJsonMixed(c *C) {
	alist, _ := ParseAndEval("{f3: 85 map: {f1: '(47 75) f2: 185}}")
	data := LispToJsonString(alist)
	c.Assert(data, Equals, `{"f3":85,"map":{"f1":[47,75],"f2":185}}`)
}

func (s *JsonLispSuite) TestLispWithFramesToJsonMixed(c *C) {
	structure, _ := ParseAndEval("{map: {f1: '(47 75) f2: 185} f3: 85 f4: 2.2}")
	data := LispWithFramesToJsonString(structure)
	c.Assert(data, Equals, `{"f3":85,"f4":2.2,"map":{"f1":[47,75],"f2":185}}`)
}

func (s *JsonLispSuite) TestLispToJsonNil(c *C) {
	data := LispToJsonString(nil)
	c.Assert(data, Equals, `""`)
}
