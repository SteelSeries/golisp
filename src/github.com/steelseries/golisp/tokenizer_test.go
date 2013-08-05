// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file tests the tokenizer
package golisp

import (
    . "launchpad.net/gocheck"
)

type TokenizerSuite struct {
}

var _ = Suite(&TokenizerSuite{})

func (s *TokenizerSuite) TestSingleCharSymbol(c *C) {
    t := NewMyTokenizer("a a")
    tok, lit := t.NextToken()
    c.Assert(tok, Equals, SYMBOL)
    c.Assert(lit, Equals, "a")
}

func (s *TokenizerSuite) TestSimpleMultipleCharacterSymbol(c *C) {
    t := NewMyTokenizer("abc a")
    tok, lit := t.NextToken()
    c.Assert(tok, Equals, SYMBOL)
    c.Assert(lit, Equals, "abc")
}

func (s *TokenizerSuite) TestSymbolWithDigit(c *C) {
    t := NewMyTokenizer("ab123c a")
    tok, lit := t.NextToken()
    c.Assert(tok, Equals, SYMBOL)
    c.Assert(lit, Equals, "ab123c")
}

func (s *TokenizerSuite) TestSymbolWithDashes(c *C) {
    t := NewMyTokenizer("abc-def a")
    tok, lit := t.NextToken()
    c.Assert(tok, Equals, SYMBOL)
    c.Assert(lit, Equals, "abc-def")
}

func (s *TokenizerSuite) TestSymbolWithUnderscore(c *C) {
    t := NewMyTokenizer("abc_def a")
    tok, lit := t.NextToken()
    c.Assert(tok, Equals, SYMBOL)
    c.Assert(lit, Equals, "abc_def")
}

func (s *TokenizerSuite) TestSymbolWithLeadingUnderscore(c *C) {
    t := NewMyTokenizer("_abc_def a")
    tok, lit := t.NextToken()
    c.Assert(tok, Equals, SYMBOL)
    c.Assert(lit, Equals, "_abc_def")
}

func (s *TokenizerSuite) TestSymbolWithQuestion(c *C) {
    t := NewMyTokenizer("abc? a")
    tok, lit := t.NextToken()
    c.Assert(tok, Equals, SYMBOL)
    c.Assert(lit, Equals, "abc?")
}

func (s *TokenizerSuite) TestSymbolWithBang(c *C) {
    t := NewMyTokenizer("abc! a")
    tok, lit := t.NextToken()
    c.Assert(tok, Equals, SYMBOL)
    c.Assert(lit, Equals, "abc!")
}

func (s *TokenizerSuite) TestSymbolWithStar(c *C) {
    t := NewMyTokenizer("abc* a")
    tok, lit := t.NextToken()
    c.Assert(tok, Equals, SYMBOL)
    c.Assert(lit, Equals, "abc*")
}

func (s *TokenizerSuite) TestShortNumber(c *C) {
    t := NewMyTokenizer("1 a")
    tok, lit := t.NextToken()
    c.Assert(tok, Equals, NUMBER)
    c.Assert(lit, Equals, "1")
}

func (s *TokenizerSuite) TestLongNumber(c *C) {
    t := NewMyTokenizer("1234567 a")
    tok, lit := t.NextToken()
    c.Assert(tok, Equals, NUMBER)
    c.Assert(lit, Equals, "1234567")
}

func (s *TokenizerSuite) TestHexNumber(c *C) {
    t := NewMyTokenizer("0x1f a")
    tok, lit := t.NextToken()
    c.Assert(tok, Equals, HEXNUMBER)
    c.Assert(lit, Equals, "0x1f")
}

func (s *TokenizerSuite) TestUppercaseHexNumber(c *C) {
    t := NewMyTokenizer("0x1F a")
    tok, lit := t.NextToken()
    c.Assert(tok, Equals, HEXNUMBER)
    c.Assert(lit, Equals, "0x1F")
}

func (s *TokenizerSuite) TestFloat(c *C) {
    t := NewMyTokenizer("12.345 a")
    tok, lit := t.NextToken()
    c.Assert(tok, Equals, FLOAT)
    c.Assert(lit, Equals, "12.345")
}

func (s *TokenizerSuite) TestNegativeFloat(c *C) {
    t := NewMyTokenizer("-12.345 a")
    tok, lit := t.NextToken()
    c.Assert(tok, Equals, FLOAT)
    c.Assert(lit, Equals, "-12.345")
}

func (s *TokenizerSuite) TestString(c *C) {
    t := NewMyTokenizer(`"hi" a`)
    tok, lit := t.NextToken()
    c.Assert(tok, Equals, STRING)
    c.Assert(lit, Equals, `hi`)
}

func (s *TokenizerSuite) TestQuote(c *C) {
    t := NewMyTokenizer(`' a`)
    tok, lit := t.NextToken()
    c.Assert(tok, Equals, QUOTE)
    c.Assert(lit, Equals, `'`)
}

func (s *TokenizerSuite) TestLParen(c *C) {
    t := NewMyTokenizer(`( a`)
    tok, lit := t.NextToken()
    c.Assert(tok, Equals, LPAREN)
    c.Assert(lit, Equals, `(`)
}

func (s *TokenizerSuite) TestRParen(c *C) {
    t := NewMyTokenizer(`) a`)
    tok, lit := t.NextToken()
    c.Assert(tok, Equals, RPAREN)
    c.Assert(lit, Equals, `)`)
}

func (s *TokenizerSuite) TestPeriod(c *C) {
    t := NewMyTokenizer(`. a`)
    tok, lit := t.NextToken()
    c.Assert(tok, Equals, PERIOD)
    c.Assert(lit, Equals, `.`)
}

func (s *TokenizerSuite) TestAdd(c *C) {
    t := NewMyTokenizer(`+ a`)
    tok, lit := t.NextToken()
    c.Assert(tok, Equals, SYMBOL)
    c.Assert(lit, Equals, `+`)
}

func (s *TokenizerSuite) TestSub(c *C) {
    t := NewMyTokenizer(`- a`)
    tok, lit := t.NextToken()
    c.Assert(tok, Equals, SYMBOL)
    c.Assert(lit, Equals, `-`)
}

func (s *TokenizerSuite) TestMul(c *C) {
    t := NewMyTokenizer(`* a`)
    tok, lit := t.NextToken()
    c.Assert(tok, Equals, SYMBOL)
    c.Assert(lit, Equals, `*`)
}

func (s *TokenizerSuite) TestQuo(c *C) {
    t := NewMyTokenizer(`/ a`)
    tok, lit := t.NextToken()
    c.Assert(tok, Equals, SYMBOL)
    c.Assert(lit, Equals, `/`)
}

func (s *TokenizerSuite) TestRem(c *C) {
    t := NewMyTokenizer(`% a`)
    tok, lit := t.NextToken()
    c.Assert(tok, Equals, SYMBOL)
    c.Assert(lit, Equals, `%`)
}

func (s *TokenizerSuite) TestLeq(c *C) {
    t := NewMyTokenizer(`<= a`)
    tok, lit := t.NextToken()
    c.Assert(tok, Equals, SYMBOL)
    c.Assert(lit, Equals, `<=`)
}

func (s *TokenizerSuite) TestLss(c *C) {
    t := NewMyTokenizer(`< a`)
    tok, lit := t.NextToken()
    c.Assert(tok, Equals, SYMBOL)
    c.Assert(lit, Equals, `<`)
}

func (s *TokenizerSuite) TestGeq(c *C) {
    t := NewMyTokenizer(`>= a`)
    tok, lit := t.NextToken()
    c.Assert(tok, Equals, SYMBOL)
    c.Assert(lit, Equals, `>=`)
}

func (s *TokenizerSuite) TestGtr(c *C) {
    t := NewMyTokenizer(`> a`)
    tok, lit := t.NextToken()
    c.Assert(tok, Equals, SYMBOL)
    c.Assert(lit, Equals, `>`)
}

func (s *TokenizerSuite) TestEql(c *C) {
    t := NewMyTokenizer(`== a`)
    tok, lit := t.NextToken()
    c.Assert(tok, Equals, SYMBOL)
    c.Assert(lit, Equals, `==`)
}

func (s *TokenizerSuite) TestNeq(c *C) {
    t := NewMyTokenizer(`!= a`)
    tok, lit := t.NextToken()
    c.Assert(tok, Equals, SYMBOL)
    c.Assert(lit, Equals, `!=`)
}

func (s *TokenizerSuite) TestNot(c *C) {
    t := NewMyTokenizer(`! a`)
    tok, lit := t.NextToken()
    c.Assert(tok, Equals, SYMBOL)
    c.Assert(lit, Equals, `!`)
}

func (s *TokenizerSuite) TestFalse(c *C) {
    t := NewMyTokenizer(`#f a`)
    tok, lit := t.NextToken()
    c.Assert(tok, Equals, FALSE)
    c.Assert(lit, Equals, `#f`)
}

func (s *TokenizerSuite) TestTrue(c *C) {
    t := NewMyTokenizer(`#t a`)
    tok, lit := t.NextToken()
    c.Assert(tok, Equals, TRUE)
    c.Assert(lit, Equals, `#t`)
}
