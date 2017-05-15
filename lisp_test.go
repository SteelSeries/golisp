// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file runs lisp based tests.

package golisp

import (
	"fmt"
	"os"
	"path/filepath"

	. "gopkg.in/check.v1"
)

type LispSuite struct {
}

var _ = Suite(&LispSuite{})

func loadFile(path string) error {
	absPath, err := filepath.Abs(path)
	if err != nil {
		fmt.Printf(" - Error: %s\n", err)
		return err
	}
	if !(filepath.Base(absPath) == ".") && !(filepath.Base(absPath) == "..") {
		extension := filepath.Ext(path)
		if extension == ".scm" || extension == ".lsp" {
			fmt.Printf("  Loading %s", path)
			_, err = ProcessFile(absPath)
			if err != nil {
				fmt.Printf(" - Error: %s\n", err)
				return err
			} else {
				fmt.Printf("\n")
			}
		}
	}
	return nil
}

func walkFunc(path string, info os.FileInfo, err error) error {
	return loadFile(path)
}

func (s *LispSuite) TestLisp(c *C) {
	testCommand := "(run-all-tests \"tests\" #t)"
	err := filepath.Walk(os.ExpandEnv("$GOLISPHOME/lisp"), walkFunc)
	if err != nil {
		fmt.Printf("Error loading code\n")
		return
	}

	c.Assert(err, IsNil)
	_, err = ProcessFile(os.ExpandEnv("$GOLISPHOME/tools/testing.scm"))
	c.Assert(err, IsNil)
	data, err := ParseAndEval(testCommand)
	c.Assert(err, IsNil)
	c.Assert(BooleanP(data), Equals, true)
	c.Assert(BooleanValue(data), Equals, true)
}
