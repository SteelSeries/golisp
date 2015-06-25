// Copyright 2015 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file implements the profiler support.

package golisp

import (
	"errors"
	"fmt"
	"os"
	"time"
)

var profileOutput *os.File

func StartProfiling(fname string) {
	if fname == "" {
		profileOutput = nil
	} else {
		profileOutput, err := os.Create(fname)
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

func ProfileEnter(funcType string, name string) {
	if ProfileEnabled {
		if profileOutput == nil {
			fmt.Printf("(%d enter %s %s)\n", time.Now().UnixNano(), funcType, name)
		} else {
			fmt.Fprintf(profileOutput, "(%d enter %s %s)\n", time.Now().UnixNano(), funcType, name)
		}
	}
}

func ProfileExit(funcType string, name string) {
	if ProfileEnabled {
		if profileOutput == nil {
			fmt.Printf("(%d exit  %s %s)\n", time.Now().UnixNano(), funcType, name)
		} else {
			fmt.Fprintf(profileOutput, "(%d exit  %s %s)\n", time.Now().UnixNano(), funcType, name)
		}
	}
}
