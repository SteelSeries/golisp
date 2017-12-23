// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpreter for embedding in a go program for scripting.
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
	Global = NewSymbolTableFrameBelow(nil, "SystemGlobal")
	Global.BindToProtected(Intern("nil"), EmptyCons())
	Global.BindToProtected(Intern("system-global-environment"), EnvironmentWithValue(Global))
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
	RegisterFramePrimitives()
	RegisterConcurrencyPrimitives()
	RegisterEnvironmentPrimitives()
	RegisterIOPrimitives()
	RegisterChannelPrimitives()
}
