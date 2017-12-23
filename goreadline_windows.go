// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpreter for embedding in a go program for scripting.
// This file dummies out the readline library as it is not supported
// or required on Windows.

package golisp

import (
	"bufio"
	"fmt"
	"os"
)

func ReadLine(prompt *string) *string {
	if prompt != nil {
		fmt.Printf("%s", *prompt)
	}

	s := bufio.NewScanner(os.Stdin)
	s.Scan()

	result := s.Text()
	return &result
}

func AddHistory(line string) {
	// TODO
}

func ClearHistory() {
	// TODO
}

func WriteHistoryToFile(fileName string) {
	// TODO
}

func LoadHistoryFromFile(fileName string) {
	// TODO
}

func TruncateHistoryFile(fileName string, left int) {
	// TODO
}
