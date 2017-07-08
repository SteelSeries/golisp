// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file implements Json<->Lisp conversions using frames.

package golisp

import (
	"encoding/json"
	"fmt"
	"math"
	"reflect"
	"strings"
)

func JsonToLispWithFrames(json interface{}) *Data {
	if json == nil {
		return nil
	}

	rt := reflect.TypeOf(json)
	rv := reflect.ValueOf(json)
	rtKind := rt.Kind()

	// maps with string keys get converted to frames
	if rtKind == reflect.Map && reflect.Type.Key(rt).Kind() == reflect.String {
		m := &FrameMap{}
		m.Data = make(FrameMapData, rv.Len())
		for _, key := range rv.MapKeys() {
			val := rv.MapIndex(key)
			value := JsonToLispWithFrames(val.Interface())
			m.Data[fmt.Sprintf("%s:", key.Interface().(string))] = value
		}
		return FrameWithValue(m)
	}

	// slices and arrays get converted to lists
	if rtKind == reflect.Array || rtKind == reflect.Slice {
		var ary *Data
		for i := 0; i < rv.Len(); i++ {
			val := rv.Index(i).Interface()
			value := JsonToLispWithFrames(val)
			ary = Cons(value, ary)
		}
		return Reverse(ary)
	}

	// handle conversion for all primitives
	switch rtKind {
	case
		reflect.Int,
		reflect.Int8,
		reflect.Int16,
		reflect.Int32,
		reflect.Int64,
		reflect.Uint,
		reflect.Uint8,
		reflect.Uint16,
		reflect.Uint32,
		reflect.Uint64,
		reflect.Uintptr:
		var intValue int64
		intValue = reflect.ValueOf(json).Convert(reflect.TypeOf(intValue)).Int()
		return IntegerWithValue(intValue)
	case reflect.Float32, reflect.Float64:
		var floatValue float64
		floatValue = reflect.ValueOf(json).Convert(reflect.TypeOf(floatValue)).Float()
		if math.Trunc(floatValue) == floatValue {
			return IntegerWithValue(int64(floatValue))
		} else {
			return FloatWithValue(float32(floatValue))
		}
	case reflect.String:
		return StringWithValue(rv.String())
	case reflect.Bool:
		return BooleanWithValue(rv.Bool())
	}

	return nil
}

func JsonStringToLispWithFrames(jsonData string) (result *Data) {
	b := []byte(jsonData)
	var data interface{}
	err := json.Unmarshal(b, &data)
	if err != nil {
		fmt.Printf("Returning empty frame because of badly formed json: '%s'\n --> %v\n", jsonData, err)
		m := FrameMap{}
		m.Data = make(FrameMapData, 0)
		return FrameWithValue(&m)
	}
	return JsonToLispWithFrames(data)
}

func LispWithFramesToJson(d *Data) (result interface{}) {
	if d == nil {
		return ""
	}

	if IntegerP(d) {
		return IntegerValue(d)
	}

	if FloatP(d) {
		return FloatValue(d)
	}

	if StringP(d) || SymbolP(d) {
		return StringValue(d)
	}

	if BooleanP(d) {
		return BooleanValue(d)
	}

	if PairP(d) {
		ary := make([]interface{}, 0, Length(d))
		for c := d; NotNilP(c); c = Cdr(c) {
			ary = append(ary, LispWithFramesToJson(Car(c)))
		}
		return ary
	}

	if ObjectP(d) && ObjectType(d) == "[]byte" {
		ary := make([]interface{}, 0, Length(d))
		for _, b := range *(*[]byte)(ObjectValue(d)) {
			ary = append(ary, float64(b))
		}
		return ary
	}

	if FrameP(d) {
		dict := make(map[string]interface{}, Length(d))
		frame := FrameValue(d)
		frame.Mutex.RLock()
		for k, v := range frame.Data {
			if !FunctionP(v) {
				dict[strings.TrimRight(k, ":")] = LispWithFramesToJson(v)
			}
		}
		frame.Mutex.RUnlock()
		return dict
	}

	return ""
}

func LispWithFramesToJsonString(d *Data) (result string) {
	temp := LispWithFramesToJson(d)
	j, err := json.Marshal(temp)
	if err == nil {
		return string(j)
	} else {
		return ""
	}
}
