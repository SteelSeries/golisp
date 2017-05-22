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
	"strings"
)

func JsonToLispWithFrames(json interface{}) (result *Data) {
	mapValue, ok := json.(map[string]interface{})
	if ok {
		m := FrameMap{}
		m.Data = make(FrameMapData, len(mapValue))
		for key, val := range mapValue {
			value := JsonToLispWithFrames(val)
			m.Data[fmt.Sprintf("%s:", key)] = value
		}
		return FrameWithValue(&m)
	}

	arrayValue, ok := json.([]interface{})
	if ok {
		ary := make([]*Data, len(arrayValue))
		for index, val := range arrayValue {
			value := JsonToLispWithFrames(val)
			ary[index] = value
		}
		return VectorWithValue(ary)
	}

	numValue, ok := json.(float64)
	if ok {
		if math.Trunc(numValue) == numValue {
			return IntegerWithValue(int64(numValue))
		} else {
			return FloatWithValue(float64(numValue))
		}
	}

	floatValue, ok := json.(float64)
	if ok {
		if math.Mod(floatValue, 1) == 0 {
			return IntegerWithValue(int64(floatValue))
		} else {
			return FloatWithValue(float64(floatValue))
		}
	}

	strValue, ok := json.(string)
	if ok {
		return StringWithValue(strValue)
	}

	boolValue, ok := json.(bool)
	if ok {
		return BooleanWithValue(boolValue)
	}

	return
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

	if VectorP(d) {
		ary := make([]interface{}, 0, Length(d))
		v := VectorValue(d)
		for _, val := range v {
			ary = append(ary, LispWithFramesToJson(val))
		}
		return ary
	}

	if ObjectP(d) && ObjectType(d) == "[]byte" {
		ary := make([]interface{}, 0, Length(d))
		for _, b := range *(*[]byte)(ObjectValue(d)) {
			ary = append(ary, b)
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
