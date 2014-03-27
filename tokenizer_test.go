// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file tests the tokenizer.

package golisp

import (
	. "launchpad.net/gocheck"
)

type TokenizerSuite struct {
}

var _ = Suite(&TokenizerSuite{})

func (s *TokenizerSuite) TestSingleCharSymbol(c *C) {
	t := NewTokenizer("a a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, "a")
}

func (s *TokenizerSuite) TestSimpleMultipleCharacterSymbol(c *C) {
	t := NewTokenizer("abc a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, "abc")
}

func (s *TokenizerSuite) TestSymbolWithDigit(c *C) {
	t := NewTokenizer("ab123c a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, "ab123c")
}

func (s *TokenizerSuite) TestSymbolWithDashes(c *C) {
	t := NewTokenizer("abc-def a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, "abc-def")
}

func (s *TokenizerSuite) TestSymbolWithUnderscore(c *C) {
	t := NewTokenizer("abc_def a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, "abc_def")
}

func (s *TokenizerSuite) TestSymbolWithLeadingUnderscore(c *C) {
	t := NewTokenizer("_abc_def a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, "_abc_def")
}

func (s *TokenizerSuite) TestSymbolWithQuestion(c *C) {
	t := NewTokenizer("abc? a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, "abc?")
}

func (s *TokenizerSuite) TestSymbolWithBang(c *C) {
	t := NewTokenizer("abc! a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, "abc!")
}

func (s *TokenizerSuite) TestSymbolWithStar(c *C) {
	t := NewTokenizer("abc* a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, "abc*")
}

func (s *TokenizerSuite) TestSymbolWithGt(c *C) {
	t := NewTokenizer("ab->c a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, "ab->c")
}

func (s *TokenizerSuite) TestShortNumber(c *C) {
	t := NewTokenizer("1 a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, NUMBER)
	c.Assert(lit, Equals, "1")
}

func (s *TokenizerSuite) TestLongInteger(c *C) {
	t := NewTokenizer("1234567 a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, NUMBER)
	c.Assert(lit, Equals, "1234567")
}

func (s *TokenizerSuite) TestNegativeInteger(c *C) {
	t := NewTokenizer("-42 a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, NUMBER)
	c.Assert(lit, Equals, "-42")
}

func (s *TokenizerSuite) TestHexInteger(c *C) {
	t := NewTokenizer("0x1f a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, HEXNUMBER)
	c.Assert(lit, Equals, "0x1f")
}

func (s *TokenizerSuite) TestUppercaseHexInteger(c *C) {
	t := NewTokenizer("0x1F a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, HEXNUMBER)
	c.Assert(lit, Equals, "0x1F")
}

func (s *TokenizerSuite) TestFloat(c *C) {
	t := NewTokenizer("12.345 a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, FLOAT)
	c.Assert(lit, Equals, "12.345")
}

func (s *TokenizerSuite) TestNegativeFloat(c *C) {
	t := NewTokenizer("-12.345 a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, FLOAT)
	c.Assert(lit, Equals, "-12.345")
}

func (s *TokenizerSuite) TestString(c *C) {
	t := NewTokenizer(`"hi" a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, STRING)
	c.Assert(lit, Equals, `hi`)
}

func (s *TokenizerSuite) TestStringWithEscapedChar(c *C) {
	t := NewTokenizer(`"hi\""" a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, STRING)
	c.Assert(lit, Equals, `hi"`)
}

func (s *TokenizerSuite) TestQuote(c *C) {
	t := NewTokenizer(`'a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, QUOTE)
	c.Assert(lit, Equals, `'`)
}

func (s *TokenizerSuite) TestBackquote(c *C) {
	t := NewTokenizer("`a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, BACKQUOTE)
	c.Assert(lit, Equals, "`")
}

func (s *TokenizerSuite) TestComma(c *C) {
	t := NewTokenizer(",a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, COMMA)
	c.Assert(lit, Equals, ",")
}

func (s *TokenizerSuite) TestCommaAt(c *C) {
	t := NewTokenizer(",@a")
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, COMMAAT)
	c.Assert(lit, Equals, ",@")
}

func (s *TokenizerSuite) TestLParen(c *C) {
	t := NewTokenizer(`( a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, LPAREN)
	c.Assert(lit, Equals, `(`)
}

func (s *TokenizerSuite) TestRParen(c *C) {
	t := NewTokenizer(`) a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, RPAREN)
	c.Assert(lit, Equals, `)`)
}

func (s *TokenizerSuite) TestLBracket(c *C) {
	t := NewTokenizer(`[ a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, LBRACKET)
	c.Assert(lit, Equals, `[`)
}

func (s *TokenizerSuite) TestRBracket(c *C) {
	t := NewTokenizer(`] a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, RBRACKET)
	c.Assert(lit, Equals, `]`)
}

func (s *TokenizerSuite) TestPeriod(c *C) {
	t := NewTokenizer(`. a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, PERIOD)
	c.Assert(lit, Equals, `.`)
}

func (s *TokenizerSuite) TestAdd(c *C) {
	t := NewTokenizer(`+ a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, `+`)
}

func (s *TokenizerSuite) TestSub(c *C) {
	t := NewTokenizer(`- a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, `-`)
}

func (s *TokenizerSuite) TestMul(c *C) {
	t := NewTokenizer(`* a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, `*`)
}

func (s *TokenizerSuite) TestQuo(c *C) {
	t := NewTokenizer(`/ a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, `/`)
}

func (s *TokenizerSuite) TestRem(c *C) {
	t := NewTokenizer(`% a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, `%`)
}

func (s *TokenizerSuite) TestLeq(c *C) {
	t := NewTokenizer(`<= a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, `<=`)
}

func (s *TokenizerSuite) TestLss(c *C) {
	t := NewTokenizer(`< a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, `<`)
}

func (s *TokenizerSuite) TestGeq(c *C) {
	t := NewTokenizer(`>= a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, `>=`)
}

func (s *TokenizerSuite) TestGtr(c *C) {
	t := NewTokenizer(`> a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, `>`)
}

func (s *TokenizerSuite) TestEql(c *C) {
	t := NewTokenizer(`== a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, `==`)
}

func (s *TokenizerSuite) TestDoubleEqual(c *C) {
	t := NewTokenizer(`== a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, `==`)
}

func (s *TokenizerSuite) TestSingleEqual(c *C) {
	t := NewTokenizer(`= a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, `=`)
}

func (s *TokenizerSuite) TestNeq(c *C) {
	t := NewTokenizer(`!= a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, `!=`)
}

func (s *TokenizerSuite) TestNot(c *C) {
	t := NewTokenizer(`! a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, SYMBOL)
	c.Assert(lit, Equals, `!`)
}

func (s *TokenizerSuite) TestFalse(c *C) {
	t := NewTokenizer(`#f a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, FALSE)
	c.Assert(lit, Equals, `#f`)
}

func (s *TokenizerSuite) TestTrue(c *C) {
	t := NewTokenizer(`#t a`)
	tok, lit := t.NextToken()
	c.Assert(tok, Equals, TRUE)
	c.Assert(lit, Equals, `#t`)
}
