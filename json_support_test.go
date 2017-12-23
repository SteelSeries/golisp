// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpreter for embedding in a go program for scripting.
// This file tests the Json<->Lisp support.

package golisp

import . "gopkg.in/check.v1"

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
	jsonData := `{"map": {"f1": [47, 75], "f2": 185}, "f3": 85}`
	sexpr := JsonStringToLisp(jsonData)

	expected := Acons(StringWithValue("map"),
		Acons(StringWithValue("f1"),
			InternalMakeList(IntegerWithValue(47), IntegerWithValue(75)),
			Acons(StringWithValue("f2"),
				IntegerWithValue(185), nil)),
		Acons(StringWithValue("f3"),
			IntegerWithValue(85), nil))

	c.Assert(IsEqual(sexpr, expected), Equals, true)
}

func (s *JsonLispSuite) TestJsonToLispMixedWithFrames(c *C) {
	jsonData := `{"map": {"f1": [47, 75], "f2": 185}, "f3": 85, "f4": 2.2}`
	sexpr := JsonStringToLispWithFrames(jsonData)
	expected, _ := ParseAndEval("{map: {f1: '(47 75) f2: 185} f3: 85 f4: 2.2}")
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
	alist := Acons(StringWithValue("map"), Acons(StringWithValue("f1"), InternalMakeList(IntegerWithValue(47), IntegerWithValue(75)), Acons(StringWithValue("f2"), IntegerWithValue(185), nil)), Acons(StringWithValue("f3"), IntegerWithValue(85), nil))
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

type intType int

func (s *JsonLispSuite) TestJsonToLispWithFramesMap(c *C) {
	a := int(63)
	b := &a
	_c := (*int)(nil)
	d_p := (*int)(nil)
	d := &d_p
	testMap := map[string]interface{}{
		"float64":    float64(1.1),
		"float64int": float64(1.0),
		"float32":    float32(1.1),
		"float32int": float32(1.0),

		"int":     int(1),
		"int8":    int8(1),
		"int16":   int16(1),
		"int32":   int32(1),
		"int64":   int64(1),
		"uint":    uint(1),
		"uint8":   uint8(1),
		"uint16":  uint16(1),
		"uint32":  uint32(1),
		"uint64":  uint64(1),
		"uintptr": uintptr(1),

		"int_ptr":    &a,
		"int_ptrptr": &b,

		"int_type": intType(1),

		"bool":   true,
		"string": "asdf",

		"nil":       nil,
		"nilptr":    &_c,
		"nilptrptr": &d,

		"bad_map":       map[int]int{1: 2},
		"good_map":      map[string]int{"1": 2},
		"interface_map": map[string]interface{}{"abc": 123},

		"slice_interface": []interface{}{1, 2, true},
		"slice_typed":     []string{"a", "b", "c"},
		"slice_2d":        [][]string{{"a", "b"}, {"c", "d"}},
		"slice_pointer":   &[]interface{}{1, 2, true},
	}
	expected, _ := ParseAndEval(`
		{bad_map: () bool: #t float32: 1.1 float32int: 1 float64: 1.1 float64int: 1 good_map: {(intern "1:") 2} int16: 1 int32: 1 int64: 1 int8: 1 int: 1 int_ptr: 63 int_ptrptr: 63
		int_type: 1 interface_map: {abc: 123} nil: () nilptr: () nilptrptr: () slice_2d: '(("a" "b") ("c" "d")) slice_interface: '(1 2 #t) slice_pointer: '(1 2 #t)
		slice_typed: '("a" "b" "c") string: "asdf" uint16: 1 uint32: 1 uint64: 1 uint8: 1 uint: 1 uintptr: 1}`)
	data := JsonToLispWithFrames(testMap)
	c.Assert(IsEqual(data, expected), Equals, true)
}

type imageSyncFrame struct {
	Image []interface{} `json:"image,omitempty"`
	Delay int           `json:",string"`
}

type imageSyncGif struct {
	Frames []imageSyncFrame `json:"frames"`
}

type imageSync struct {
	Gif      imageSyncGif `json:"gif_data"`
	Enabled  int          `json:"enabled,string,omitempty"`
	Str      string       `json:",omitempty"`
	private  string
	Inter    interface{}   `json:"inter,omitempty"`
	NilSlice []interface{} `json:"nilslice"`
}

func (s *JsonLispSuite) TestJsonToLispWithFramesStruct(c *C) {
	jsonData := imageSync{
		private:  "sss",
		Str:      "",
		Enabled:  6,
		Inter:    struct{}{},
		NilSlice: nil,
		Gif: imageSyncGif{
			Frames: []imageSyncFrame{
				imageSyncFrame{
					Delay: 1,
					Image: []interface{}{1, 3, 4},
				},
				imageSyncFrame{
					Delay: 2,
					Image: []interface{}{},
				},
				imageSyncFrame{
					Delay: 3,
					Image: nil,
				},
			},
		},
	}
	data := JsonToLispWithFrames(jsonData)
	expected, _ := ParseAndEval(`{enabled: "6" gif_data: {frames: (list {Delay: "1" image: '(1 3 4)} {Delay: "2"} {Delay: "3"})} inter: {} nilslice: ()}`)
	c.Assert(IsEqual(data, expected), Equals, true)
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
