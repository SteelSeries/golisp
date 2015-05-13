// Copyright 2015 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file implements a way of providing loggers for GoLisp to write to

package golisp

import (
	"log"
	"os"
)

var (
	loggers []*log.Logger
)

func init() {
	// Initialize a standard logger to stdout
	loggers = make([]*log.Logger, 1)
	loggers[0] = log.New(os.Stdout, "GoLisp: ", log.Lshortfile)
}

func LogPrintf(format string, a ...interface{}) {
	for _, logger := range loggers {
		logger.Printf(format, a...)
	}
}

func LogPrint(a ...interface{}) {
	for _, logger := range loggers {
		logger.Print(a...)
	}
}

func LogPrintln(a ...interface{}) {
	for _, logger := range loggers {
		logger.Println(a...)
	}
}

func AddLog(newLog *log.Logger) {
	loggers = append(loggers, newLog)
}
