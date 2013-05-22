// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file implements json<->lisp conversions

package golisp

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
        return NumberWithValue(uint32(intValue))
    }

    strValue, ok := json.(string)
    if ok {
        return StringWithValue(strValue)
    }

    return
}

func LispToJson(d *Data) (result interface{}) {
    if NilP(d) && !AlistP(d) && !PairP(d) {
        return ""
    }

    if NumberP(d) {
        return NumericValue(d)
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

func TransformJson(xform *Data, jsonNode *Data, parentNode *Data) (xformedJson *Data, err error) {
    var transformFunction *Data
    var newData *Data

    args := InternalMakeList(jsonNode, parentNode)
    transformFunction, err = Eval(xform, Global)
    if err != nil {
        return
    }
    _, err = Apply(transformFunction, args, Global)
    if err != nil {
        return
    }
    xformedJson = newData
    return
}
