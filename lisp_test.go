// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file runs lisp based tests.

package golisp

import (
    . "launchpad.net/gocheck"
    "path/filepath"
)

type LispSuite struct {
}

var _ = Suite(&LispSuite{})

func (s *LispSuite) TestLisp(c *C) {
    files, err := filepath.Glob("tests/*.lsp")
    if err != nil {
        c.Fail()
    }
    for _, f := range files {
        c.Logf("Loading %s\n", f)
        _, err := ProcessFile(f)
        if err != nil {
            c.Logf("Error: %s\n", err)
        }
    }
    PrintTestResults()
}
