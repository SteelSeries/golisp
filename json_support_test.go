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
}

var _ = Suite(&JsonLispSuite{})

func (s *JsonLispSuite) TestJsonToLispMap(c *C) {
	jsonData := `{"map": 1}`
	sexpr := JsonStringToLisp(jsonData)
	expected := Acons(StringWithValue("map"), IntegerWithValue(1), nil)

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
	expected := Acons(StringWithValue("map"), BooleanWithValue(true), nil)

	c.Assert(IsEqual(sexpr, expected), Equals, true)
}

func (s *JsonLispSuite) TestJsonToLispMixed(c *C) {
	jsonData := `{"map": {"f1": [47, 75], "f2": 185}, "f3": 85, "f4": 2.2}`
	sexpr := JsonStringToLisp(jsonData)

	expected := Acons(StringWithValue("map"),
		Acons(StringWithValue("f1"),
			InternalMakeList(IntegerWithValue(47), IntegerWithValue(75)),
			Acons(StringWithValue("f2"),
				IntegerWithValue(185), nil)),
		Acons(StringWithValue("f3"),
			IntegerWithValue(85),
		Acons(StringWithValue("f4"), 			FloatWithValue(2.2), nil)))

	c.Assert(IsEqual(sexpr, expected), Equals, true)
}

func (s *JsonLispSuite) TestJsonToLispMixedWithFrames(c *C) {
	jsonData := `{"map": {"f1": [47, 75], "f2": 185}, "f3": 85}`
	sexpr := JsonStringToLispWithFrames(jsonData)
	expected, _ := ParseAndEval("{map: {f1: '(47 75) f2: 185} f3: 85}")
	c.Assert(IsEqual(sexpr, expected), Equals, true)
}

func (s *JsonLispSuite) TestJsonToLispIllegal(c *C) {
	c.Assert(func() { JsonStringToLisp("hello") }, PanicMatches, `Badly formed json: 'hello'`)
}

func (s *JsonLispSuite) TestLispToJsonMap(c *C) {
	alist := Acons(StringWithValue("map"), IntegerWithValue(1), nil)
	data := LispToJsonString(alist)
	c.Assert(data, Equals, `{"map":1}`)
}

func (s *JsonLispSuite) TestLispToJsonArray(c *C) {
	alist := InternalMakeList(IntegerWithValue(1), IntegerWithValue(2), StringWithValue("hi"))
	data := LispToJsonString(alist)
	c.Assert(data, Equals, `[1,2,"hi"]`)
}

func (s *JsonLispSuite) TestLispToJsonMixed(c *C) {
	alist := Acons(StringWithValue("map"), Acons(StringWithValue("f1"), InternalMakeList(IntegerWithValue(47), IntegerWithValue(75)), Acons(StringWithValue("f2"), IntegerWithValue(185), nil)), Acons(StringWithValue("f3"), IntegerWithValue(85), Acons(StringWithValue("f4"), FloatWithValue(2.2), nil)))
	data := LispToJsonString(alist)
	c.Assert(data, Equals, `{"f3":85,"f4":2.2,"map":{"f1":[47,75],"f2":185}}`)
}

func (s *JsonLispSuite) TestLispWithFramesToJsonMixed(c *C) {
	structure, _ := ParseAndEval("{map: {f1: '(47 75) f2: 185} f3: 85}")
	data := LispWithFramesToJsonString(structure)
	c.Assert(data, Equals, `{"f3":85,"map":{"f1":[47,75],"f2":185}}`)
}

func (s *JsonLispSuite) TestLispToJsonNil(c *C) {
	data := LispToJsonString(nil)
	c.Assert(data, Equals, `""`)
}

// func (s *JsonLispSuite) TestSimpleJsonTransformation(c *C) {
// 	jsonData := Acons(StringWithValue("map"), Acons(StringWithValue("f1"), InternalMakeList(IntegerWithValue(47), IntegerWithValue(75)), Acons(StringWithValue("f2"), IntegerWithValue(185), nil)), Acons(StringWithValue("f3"), IntegerWithValue(85), nil))

// 	xform, err := Parse(`(lambda (node parent) (acons "f3" 42 parent))`)
// 	c.Assert(err, IsNil)
// 	parent := jsonData
// 	pair, _ := Assoc(StringWithValue("f3"), jsonData)
// 	_, err = TransformJson(xform, Cdr(pair), parent)
// 	c.Assert(err, IsNil)

// 	var newNode *Data
// 	newNode, err = Assoc(StringWithValue("f3"), jsonData)
// 	c.Assert(err, IsNil)
// 	c.Assert(IntegerValue(Cdr(newNode)), Equals, int64(42))
// }

// func (s *JsonLispSuite) TestSimpleJsonTransformationReturnsNewValue(c *C) {
// 	jsonData := Acons(StringWithValue("map"), Acons(StringWithValue("f1"), InternalMakeList(IntegerWithValue(47), IntegerWithValue(75)), Acons(StringWithValue("f2"), IntegerWithValue(185), nil)), Acons(StringWithValue("f3"), IntegerWithValue(85), nil))

// 	xform, err := Parse(`(lambda (node parent) (+ node 5))`)
// 	c.Assert(err, IsNil)
// 	parent := jsonData
// 	pair, _ := Assoc(StringWithValue("f3"), jsonData)
// 	newValue, err := TransformJson(xform, Cdr(pair), parent)
// 	c.Assert(err, IsNil)

// 	c.Assert(IntegerValue(newValue), Equals, int64(90))
// }

// func (s *JsonLispSuite) TestMoreComplexJsonTransformation(c *C) {
// 	jsonData := Acons(StringWithValue("map"), Acons(StringWithValue("f1"), InternalMakeList(IntegerWithValue(47), IntegerWithValue(75)), Acons(StringWithValue("f2"), IntegerWithValue(185), nil)), Acons(StringWithValue("f3"), IntegerWithValue(85), nil))

// 	xform, err := Parse(`(lambda (node parent) (acons "f3" '(1 2 3) parent))`)
// 	c.Assert(err, IsNil)
// 	parent := jsonData
// 	pair, _ := Assoc(StringWithValue("f3"), jsonData)
// 	_, err = TransformJson(xform, Cdr(pair), parent)
// 	c.Assert(err, IsNil)

// 	var newNode *Data
// 	newNode, err = Assoc(StringWithValue("f3"), jsonData)
// 	c.Assert(err, IsNil)
// 	c.Assert(IsEqual(Cdr(newNode), InternalMakeList(IntegerWithValue(1), IntegerWithValue(2), IntegerWithValue(3))), Equals, true)
// }

// func (s *JsonLispSuite) TestEvenMoreComplexJsonTransformation(c *C) {
// 	jsonData := Acons(StringWithValue("map"), Acons(StringWithValue("f1"), InternalMakeList(IntegerWithValue(47), IntegerWithValue(75)), Acons(StringWithValue("f2"), IntegerWithValue(185), nil)), Acons(StringWithValue("f3"), IntegerWithValue(85), nil))

// 	xform, err := Parse(`(lambda (node parent) (acons "f3" (acons "a" (+ node 1) nil) parent))`)
// 	c.Assert(err, IsNil)
// 	parent := jsonData
// 	pair, _ := Assoc(StringWithValue("f3"), jsonData)
// 	_, err = TransformJson(xform, Cdr(pair), parent)
// 	c.Assert(err, IsNil)

// 	var newNode *Data
// 	var newerNode *Data
// 	newNode, err = Assoc(StringWithValue("f3"), jsonData)
// 	c.Assert(err, IsNil)
// 	newerNode, err = Assoc(StringWithValue("a"), Cdr(newNode))
// 	c.Assert(err, IsNil)
// 	c.Assert(IntegerValue(Cdr(newerNode)), Equals, int64(86))
// }
