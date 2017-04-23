// Copyright 2017 Dave Astels.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the byte code VM.

package golisp

const (
	LVAR = iota
	LSET
	GVAR
	GSET
	POP
	CONST
	JUMP
	FJUMP
	TJUMP
	SAVE
	RETURN
	CALLJ
	ARGS
	ARGSDOT
	FN
	PRIM
	SET_CC
	CC
	SCHEME_READ
	NEWLINE
	CAR
	CDR
	CADR
	NOT
	LIST1
	COMPILER
	DISPLAY
	WRITE
	RANDOM
	PLUS
	MINUS
	TIMES
	DIVIDE
	LT
	GT
	LTEQ
	GTEQ
	NEQ
	EQ_SIGN
	CONS
	LIST2
	NAME_BANG
	EQ
	EQUAL
	EQL
	LIST3
	TRUE
	FALSE
	NIL
	MINUS_1
	ZERO
	ONE
	TWO
	HALT
)

func Machine(f *CompiledFunction) {
	var code = f.FnCode
	var pc = 0
	var env *SymbolTableFrame = nil
	var stack = 0
}
