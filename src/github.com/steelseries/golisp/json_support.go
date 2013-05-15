// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file implements json<->lisp conversions

package golisp

import (
//    "errors"
//    "fmt"
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
        return NumberWithValue(int(intValue))
    }

    return
}

func LispToJson(d *Data) (result interface{}) {
    return
}
