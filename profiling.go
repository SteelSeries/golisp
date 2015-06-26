// Copyright 2015 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file implements the profiler support.

package golisp

import (
	"fmt"
	"os"
	"time"
)

var profileOutput *os.File = nil
var ProfileEnabled = false
var ProfileGUID int64 = 0

func StartProfiling(fname string) {
	ProfileGUID = 0
	if fname == "" {
		profileOutput = nil
	} else {
		var err error
		profileOutput, err = os.Create(fname)
		if err != nil {
			panic(fmt.Sprintf("Profiler: %s could not be opened.", fname))
		}
	}
	ProfileEnabled = true
}

func EndProfiling() {
	ProfileEnabled = false
	if profileOutput != nil {
		profileOutput.Close()
	}
}

func ProfileEnter(funcType string, name string, guid int64) {
	if ProfileEnabled {
		msg := fmt.Sprintf("(%d %d enter %s %s)\n", time.Now().UnixNano(), guid, funcType, name)
		if profileOutput == nil {
			fmt.Printf(msg)
		} else {
			fmt.Fprintf(profileOutput, msg)
		}
	}
}

func ProfileExit(funcType string, name string, guid int64) {
	if ProfileEnabled {
		msg := fmt.Sprintf("(%d %d exit  %s %s)\n", time.Now().UnixNano(), guid, funcType, name)
		if profileOutput == nil {
			fmt.Printf(msg)
		} else {
			fmt.Fprintf(profileOutput, msg)
		}
	}
}
