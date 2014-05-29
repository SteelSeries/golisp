// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file implements Json<->Lisp conversions.

package golisp

import (
	"encoding/json"
	"errors"
	"fmt"
)

func JsonToLisp(json interface{}) (result *Data) {
	mapValue, ok := json.(map[string]interface{})
	if ok {
		var alist *Data
		for key, val := range mapValue {
			value := JsonToLisp(val)
			alist = Acons(StringWithValue(key), value, alist)
		}
		return alist
	}

	arrayValue, ok := json.([]interface{})
	if ok {
		var ary *Data
		for _, val := range arrayValue {
			value := JsonToLisp(val)
			ary = Cons(value, ary)
		}
		return Reverse(ary)
	}

	intValue, ok := json.(float64)
	if ok {
		return IntegerWithValue(int64(intValue))
	}

	strValue, ok := json.(string)
	if ok {
		return StringWithValue(strValue)
	}

	return
}

func JsonStringToLisp(jsonData string) (result *Data) {
	b := []byte(jsonData)
	var data interface{}
	err := json.Unmarshal(b, &data)
	if err != nil {
		panic(errors.New(fmt.Sprintf("Badly formed json: '%s'", jsonData)))
	}
	return JsonToLisp(data)
}

func LispToJson(d *Data) (result interface{}) {
	if d == nil {
		return ""
	}

	if IntegerP(d) {
		return IntegerValue(d)
	}

	if StringP(d) {
		return StringValue(d)
	}

	if PairP(d) {
		ary := make([]interface{}, 0, Length(d))
		for c := d; NotNilP(c); c = Cdr(c) {
			ary = append(ary, LispToJson(Car(c)))
		}
		return ary
	}

	if AlistP(d) {
		dict := make(map[string]interface{}, Length(d))
		for c := d; NotNilP(c); c = Cdr(c) {
			pair := Car(c)
			dict[StringValue(Car(pair))] = LispToJson(Cdr(pair))
		}
		return dict
	}

	return ""
}

func LispToJsonString(d *Data) (result string) {
	temp := LispToJson(d)
	j, err := json.Marshal(temp)
	if err == nil {
		return string(j)
	} else {
		return ""
	}
}

func TransformJson(xform *Data, jsonNode *Data, parentNode *Data) (xformedJson *Data, err error) {
	var transformFunction *Data
	var newData *Data

	args := InternalMakeList(jsonNode, parentNode)
	transformFunction, err = Eval(xform, Global)
	if err != nil {
		return
	}
	newData, err = Apply(transformFunction, args, Global)
	if err != nil {
		return
	}
	xformedJson = newData
	return
}
