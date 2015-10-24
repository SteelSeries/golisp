// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file implements Json<->Lisp conversions.
// The alist based json representtion has been removed, replaced by the frame based representation.
// These functions now simply forward to the frame based equivalents.

package golisp

func JsonToLisp(json interface{}) (result *Data) {
	return JsonToLispWithFrames(json)
}

func JsonStringToLisp(jsonData string) (result *Data) {
	return JsonStringToLispWithFrames(jsonData)
}

func LispToJson(d *Data) (result interface{}) {
	return LispWithFramesToJson(d)
}

func LispToJsonString(d *Data) (result string) {
	return LispWithFramesToJsonString(d)
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
