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

func jsonIsEmptyValue(data *Data) bool {
	switch {
	case NilP(data):
		return true
	case StringP(data):
		return len(StringValue(data)) == 0
	case IntegerP(data):
		return IntegerValue(data) == 0
	case FloatP(data):
		return FloatValue(data) == 0.0
	case BooleanP(data):
		return !BooleanValue(data)
	}
	return false
}

// adapted from golang's encoding/json
func jsonToLispWithFramesStruct(rv reflect.Value) *Data {
	rt := rv.Type()
	m := &FrameMap{}
	m.Data = make(FrameMapData, rv.NumField())

	for i := 0; i < rv.NumField(); i++ {
		sf := rt.Field(i)

		// unexported
		if sf.PkgPath != "" && (!sf.Anonymous || sf.Type.Kind() != reflect.Struct) {
			continue
		}

		tag := sf.Tag.Get("json")
		var tagOpt []string

		if tag == "-" {
			continue
		}

		tagSplit := strings.Split(tag, ",")

		if len(tagSplit) > 1 {
			tag = tagSplit[0]
			tagOpt = tagSplit[1:]
		}

		omitEmpty := false
		encodeString := false

		for _, opt := range tagOpt {
			switch opt {
			case "omitempty":
				omitEmpty = true
			case "string":
				encodeString = true
			}
		}

		name := tag
		if name == "" {
			name = sf.Name
		}

		ft := sf.Type

		// follow pointer.
		for ft.Name() == "" && ft.Kind() == reflect.Ptr {
			ft = ft.Elem()
		}

		if name != "" || !sf.Anonymous || ft.Kind() != reflect.Struct {
			name = fmt.Sprintf("%s:", name)

			fv := rv.Field(i)
			data := jsonToLispWithFramesReflect(fv)

			if !omitEmpty || !jsonIsEmptyValue(data) {
				if encodeString {
					switch {
					case StringP(data):
						str, err := json.Marshal(StringValue(data))
						if err == nil {
							data = StringWithValue(string(str))
						}
					case IntegerP(data):
						data = StringWithValue(fmt.Sprintf("%d", IntegerValue(data)))
					case FloatP(data):
						data = StringWithValue(fmt.Sprintf("%f", FloatValue(data)))
					case BooleanP(data):
						data = StringWithValue(fmt.Sprintf("%t", BooleanValue(data)))
					}
				}

				m.Data[name] = data
			}
		}
	}

	return FrameWithValue(m)
}

func jsonToLispWithFramesReflect(rv reflect.Value) *Data {
	rt := rv.Type()
	rtKind := rt.Kind()

	// dereference pointers and interfaces
	for rtKind == reflect.Ptr || rtKind == reflect.Interface {
		if rv.IsNil() {
			return nil
		}

		rv = rv.Elem()
		rt = rv.Type()
		rtKind = rt.Kind()
	}

	switch rtKind {
	case reflect.Map:
		if reflect.Type.Key(rt).Kind() == reflect.String {
			m := &FrameMap{}
			m.Data = make(FrameMapData, rv.Len())
			for _, key := range rv.MapKeys() {
				val := rv.MapIndex(key)
				value := jsonToLispWithFramesReflect(val)
				m.Data[fmt.Sprintf("%s:", key.String())] = value
			}
			return FrameWithValue(m)
		}
	case reflect.Array, reflect.Slice:
		var ary *Data
		for i := 0; i < rv.Len(); i++ {
			val := rv.Index(i)
			value := jsonToLispWithFramesReflect(val)
			ary = Cons(value, ary)
		}
		return Reverse(ary)
	case reflect.Struct:
		return jsonToLispWithFramesStruct(rv)
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		return IntegerWithValue(rv.Int())
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
		return IntegerWithValue(int64(rv.Uint()))
	case reflect.Float32, reflect.Float64:
		floatValue := rv.Float()
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

func JsonToLispWithFrames(data interface{}) *Data {
	if data == nil {
		return nil
	}

	return jsonToLispWithFramesReflect(reflect.ValueOf(data))
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
	return jsonToLispWithFramesReflect(reflect.ValueOf(data))
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
