// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the built-in primitive functions.

package golisp

var DebugTrace = false
var LispTrace = false
var quasiquoteLevel = 1

func init() {
	InitLisp()
}

func InitLisp() {
	InitEnvironments()
	InitBuiltins()
}

func InitEnvironments() {
	TopLevelEnvironments = make(map[string]*SymbolTableFrame, 5)
	Global = NewSymbolTableFrameBelow(nil, "SystemGlobal")
	Global.Intern("nil")
	Global.BindTo(Intern("system-global-environment"), EnvironmentWithValue(Global))
}

func InitBuiltins() {
	RegisterTypePredicatePrimitives()
	RegisterMathPrimitives()
	RegisterBinaryPrimitives()
	RegisterRelativePrimitives()
	RegisterSpecialFormPrimitives()
	RegisterMacroPrimitives()
	RegisterMutatorPrimitives()
	RegisterListManipulationPrimitives()
	RegisterListAccessPrimitives()
	RegisterListFunctionsPrimitives()
	RegisterListSetPrimitives()
	RegisterAListPrimitives()
	RegisterSystemPrimitives()
	RegisterBytearrayPrimitives()
	RegisterStringPrimitives()
	RegisterDebugPrimitives()
	RegisterTestingPrimitives()
	RegisterFramePrimitives()
	RegisterConcurrencyPrimitives()
	RegisterEnvironmentPrimitives()
	RegisterIOPrimitives()
}
