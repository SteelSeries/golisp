// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpreter for embedding in a go program for scripting.
// This file tests the tokenizer.

package golisp

import (
	. "gopkg.in/check.v1"
)

type TokenizerSuite struct {
}

var _ = Suite(&TokenizerSuite{})

func (s *TokenizerSuite) TestSingleCharSymbol(c *C) {
	t := NewTokenizerFromString("a a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, "a")
}

func (s *TokenizerSuite) TestSimpleMultipleCharacterSymbol(c *C) {
	t := NewTokenizerFromString("abc a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, "abc")
}

func (s *TokenizerSuite) TestSymbolWithDigit(c *C) {
	t := NewTokenizerFromString("ab123c a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, "ab123c")
}

func (s *TokenizerSuite) TestSymbolWithDashes(c *C) {
	t := NewTokenizerFromString("abc-def a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, "abc-def")
}

func (s *TokenizerSuite) TestSymbolWithUnderscore(c *C) {
	t := NewTokenizerFromString("abc_def a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, "abc_def")
}

func (s *TokenizerSuite) TestSymbolWithLeadingUnderscore(c *C) {
	t := NewTokenizerFromString("_abc_def a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, "_abc_def")
}

func (s *TokenizerSuite) TestSymbolWithQuestion(c *C) {
	t := NewTokenizerFromString("abc? a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, "abc?")
}

func (s *TokenizerSuite) TestSymbolWithBang(c *C) {
	t := NewTokenizerFromString("abc! a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, "abc!")
}

func (s *TokenizerSuite) TestSymbolWithStar(c *C) {
	t := NewTokenizerFromString("abc* a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, "abc*")
}

func (s *TokenizerSuite) TestSymbolWithGt(c *C) {
	t := NewTokenizerFromString("ab->c a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, "ab->c")
}

func (s *TokenizerSuite) TestShortNumber(c *C) {
	t := NewTokenizerFromString("1 a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, NUMBER)
	c.Assert(lit, Equals, "1")
}

func (s *TokenizerSuite) TestLongInteger(c *C) {
	t := NewTokenizerFromString("1234567 a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, NUMBER)
	c.Assert(lit, Equals, "1234567")
}

func (s *TokenizerSuite) TestNegativeInteger(c *C) {
	t := NewTokenizerFromString("-42 a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, NUMBER)
	c.Assert(lit, Equals, "-42")
}

func (s *TokenizerSuite) TestBinaryInteger(c *C) {
	t := NewTokenizerFromString("#b1010 a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, BINARYNUMBER)
	c.Assert(lit, Equals, "1010")
}

func (s *TokenizerSuite) TestProperHexInteger(c *C) {
	t := NewTokenizerFromString("#x1f a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, HEXNUMBER)
	c.Assert(lit, Equals, "1f")
}

func (s *TokenizerSuite) TestHexInteger(c *C) {
	t := NewTokenizerFromString("0x1f a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, HEXNUMBER)
	c.Assert(lit, Equals, "1f")
}

func (s *TokenizerSuite) TestUppercaseHexInteger(c *C) {
	t := NewTokenizerFromString("0x1F a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, HEXNUMBER)
	c.Assert(lit, Equals, "1F")
}

func (s *TokenizerSuite) TestFloat(c *C) {
	t := NewTokenizerFromString("12.345 a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, FLOAT)
	c.Assert(lit, Equals, "12.345")
}

func (s *TokenizerSuite) TestNegativeFloat(c *C) {
	t := NewTokenizerFromString("-12.345 a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, FLOAT)
	c.Assert(lit, Equals, "-12.345")
}

func (s *TokenizerSuite) TestString(c *C) {
	t := NewTokenizerFromString(`"hi" a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, STRING)
	c.Assert(lit, Equals, `hi`)
}

func (s *TokenizerSuite) TestStringWithEscapedChar(c *C) {
	t := NewTokenizerFromString(`"hi\""" a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, STRING)
	c.Assert(lit, Equals, `hi"`)
}

func (s *TokenizerSuite) TestQuote(c *C) {
	t := NewTokenizerFromString(`'a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, QUOTE)
	c.Assert(lit, Equals, `'`)
}

func (s *TokenizerSuite) TestBackquote(c *C) {
	t := NewTokenizerFromString("`a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, BACKQUOTE)
	c.Assert(lit, Equals, "`")
}

func (s *TokenizerSuite) TestComma(c *C) {
	t := NewTokenizerFromString(",a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, COMMA)
	c.Assert(lit, Equals, ",")
}

func (s *TokenizerSuite) TestCommaAt(c *C) {
	t := NewTokenizerFromString(",@a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, COMMAAT)
	c.Assert(lit, Equals, ",@")
}

func (s *TokenizerSuite) TestLParen(c *C) {
	t := NewTokenizerFromString(`( a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, LPAREN)
	c.Assert(lit, Equals, `(`)
}

func (s *TokenizerSuite) TestRParen(c *C) {
	t := NewTokenizerFromString(`) a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, RPAREN)
	c.Assert(lit, Equals, `)`)
}

func (s *TokenizerSuite) TestLBracket(c *C) {
	t := NewTokenizerFromString(`[ a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, LBRACKET)
	c.Assert(lit, Equals, `[`)
}

func (s *TokenizerSuite) TestRBracket(c *C) {
	t := NewTokenizerFromString(`] a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, RBRACKET)
	c.Assert(lit, Equals, `]`)
}

func (s *TokenizerSuite) TestPeriod(c *C) {
	t := NewTokenizerFromString(`. a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, PERIOD)
	c.Assert(lit, Equals, `.`)
}

func (s *TokenizerSuite) TestAdd(c *C) {
	t := NewTokenizerFromString(`+ a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, `+`)
}

func (s *TokenizerSuite) TestSub(c *C) {
	t := NewTokenizerFromString(`- a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, `-`)
}

func (s *TokenizerSuite) TestMul(c *C) {
	t := NewTokenizerFromString(`* a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, `*`)
}

func (s *TokenizerSuite) TestQuo(c *C) {
	t := NewTokenizerFromString(`/ a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, `/`)
}

func (s *TokenizerSuite) TestRem(c *C) {
	t := NewTokenizerFromString(`% a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, `%`)
}

func (s *TokenizerSuite) TestLeq(c *C) {
	t := NewTokenizerFromString(`<= a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, `<=`)
}

func (s *TokenizerSuite) TestLss(c *C) {
	t := NewTokenizerFromString(`< a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, `<`)
}

func (s *TokenizerSuite) TestGeq(c *C) {
	t := NewTokenizerFromString(`>= a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, `>=`)
}

func (s *TokenizerSuite) TestGtr(c *C) {
	t := NewTokenizerFromString(`> a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, `>`)
}

func (s *TokenizerSuite) TestEql(c *C) {
	t := NewTokenizerFromString(`== a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, `==`)
}

func (s *TokenizerSuite) TestDoubleEqual(c *C) {
	t := NewTokenizerFromString(`== a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, `==`)
}

func (s *TokenizerSuite) TestSingleEqual(c *C) {
	t := NewTokenizerFromString(`= a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, `=`)
}

func (s *TokenizerSuite) TestNeq(c *C) {
	t := NewTokenizerFromString(`!= a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, `!=`)
}

func (s *TokenizerSuite) TestNot(c *C) {
	t := NewTokenizerFromString(`! a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, `!`)
}

func (s *TokenizerSuite) TestFalse(c *C) {
	t := NewTokenizerFromString(`#f a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, FALSE)
	c.Assert(lit, Equals, `#f`)
}

func (s *TokenizerSuite) TestTrue(c *C) {
	t := NewTokenizerFromString(`#t a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, TRUE)
	c.Assert(lit, Equals, `#t`)
}
