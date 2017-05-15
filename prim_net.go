// Copyright 2016 Dave Astels. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the network primitive functions.

package golisp

import (
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"strings"
)

func RegisterNetPrimitives() {
	MakeTypedPrimitiveFunction("net/get", "1", NetGetImpl, []uint32{StringType})
	MakeTypedPrimitiveFunction("net/post", "3", NetPostImpl, []uint32{StringType, StringType, StringType | FrameType})
	MakeTypedPrimitiveFunction("net/request", "2|3|4", NetRequestImpl, []uint32{StringType | SymbolType, StringType, FrameType, StringType | FrameType})
}

func NetGetImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	url := First(args)
	var resp *http.Response
	resp, err = http.Get(StringValue(url))
	if err != nil {
		err = ProcessError(fmt.Sprintf("Error getting: %s", err), env)
		return
	}

	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		err = ProcessError(fmt.Sprintf("Error reading net/get response body: %s", resp.Status), env)
		return
	}

	return InternalMakeList(IntegerWithValue(int64(resp.StatusCode)), StringWithValue(string(body))), nil
}

func NetPostImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	url := First(args)
	contentType := Second(args)
	content := Third(args)
	var stringContent string
	if StringP(content) {
		stringContent = StringValue(content)
	} else {
		stringContent = LispWithFramesToJsonString(content)
	}

	buf := strings.NewReader(stringContent)
	var resp *http.Response
	resp, err = http.Post(StringValue(url), StringValue(contentType), buf)
	if err != nil {
		err = ProcessError(fmt.Sprintf("Error posting: %s", err), env)
		return
	}

	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		err = ProcessError(fmt.Sprintf("Error reading net/post response body: %s", resp.Status), env)
		return
	}

	return InternalMakeList(IntegerWithValue(int64(resp.StatusCode)), StringWithValue(string(body))), nil

}

func NetRequestImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	verb := First(args)
	url := Second(args)
	var headerMap *FrameMap = nil
	var buf io.Reader

	if Length(args) >= 3 {
		headers := Third(args)
		headerMap = FrameValue(headers)

		if Length(args) == 4 {
			content := Fourth(args)
			var stringContent string
			if StringP(content) {
				stringContent = StringValue(content)
			} else {
				stringContent = LispWithFramesToJsonString(content)
			}

			buf = strings.NewReader(stringContent)
		} else {
			buf = nil
		}
	}

	client := &http.Client{}

	var resp *http.Response

	req, err := http.NewRequest(strings.ToUpper(StringValue(verb)), StringValue(url), buf)
	if err != nil {
		err = ProcessError(fmt.Sprintf("Error creating http request: %s", err), env)
		return
	}

	if Length(args) >= 3 {
		for k, v := range headerMap.Data {
			tweaked_key := strings.Title(strings.ToLower(strings.TrimRight(k, ":")))
			req.Header.Add(tweaked_key, StringValue(v))
		}
	}

	resp, err = client.Do(req)
	if err != nil {
		err = ProcessError(fmt.Sprintf("Error posting: %s", err), env)
		return
	}

	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		err = ProcessError(fmt.Sprintf("Error reading net/post response body: %s", resp.Status), env)
		return
	}

	return InternalMakeList(IntegerWithValue(int64(resp.StatusCode)), StringWithValue(string(body))), nil

}
