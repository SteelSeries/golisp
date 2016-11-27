// Copyright 2014 SteelSeries ApS.  All rights reserved.
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
	MakePrimitiveFunction("net/get", "1", NetGetImpl)
	MakePrimitiveFunction("net/post", "3", NetPostImpl)
	MakePrimitiveFunction("net/request", "2|3|4", NetRequestImpl)
}

func NetGetImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	url := First(args)
	if !StringP(url) {
		err = ProcessError(fmt.Sprintf("net/get expects its argument (a URL) to be a string, but received %s", String(url)), env)
		return
	}

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
	if !StringP(url) {
		err = ProcessError(fmt.Sprintf("net/post expects its first argument (a URL) to be a string, but received %s", String(url)), env)
		return
	}

	contentType := Second(args)
	if !StringP(contentType) {
		err = ProcessError(fmt.Sprintf("net/post expects its second argument (a content type) to be a string, but received %s", String(contentType)), env)
		return
	}
	content := Third(args)
	if !StringP(content) && !FrameP(content) {
		err = ProcessError(fmt.Sprintf("net/post expects its fourth argument (content) to be a string or frame, but received %s", String(content)), env)
		return
	}

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
	if !StringP(verb) && !SymbolP(verb) {
		err = ProcessError(fmt.Sprintf("net/request expects its first argument (an HTTP verb) to be a string or symbol, but received %s", String(verb)), env)
		return
	}

	url := Second(args)
	if !StringP(url) {
		err = ProcessError(fmt.Sprintf("net/request expects its second argument (a URL) to be a string, but received %s", String(url)), env)
		return
	}

	var headerMap *FrameMap = nil
	var buf io.Reader

	if Length(args) >= 3 {
		headers := Third(args)
		if !FrameP(headers) {
			err = ProcessError(fmt.Sprintf("net/request expects its third argument (headers) to be a frame, but received %s", String(headers)), env)
			return
		}

		headerMap = FrameValue(headers)

		if Length(args) == 4 {
			content := Fourth(args)
			if !StringP(content) && !FrameP(content) {
				err = ProcessError(fmt.Sprintf("net/request expects its fourth argument (content) to be a string or frame, but received %s", String(content)), env)
				return
			}

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
		for k, v := range *headerMap {
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
