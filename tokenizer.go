// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file implements the tokenizer.

package golisp

import (
	"fmt"
	"github.com/SteelSeries/bufrr"
	"io"
	"os"
	"strings"
	"unicode"
)

const (
	ILLEGAL = iota
	SYMBOL
	NUMBER
	HEXNUMBER
	BINARYNUMBER
	FLOAT
	STRING
	QUOTE
	BACKQUOTE
	COMMA
	COMMAAT
	LPAREN
	RPAREN
	LBRACKET
	RBRACKET
	LBRACE
	RBRACE
	PERIOD
	TRUE
	FALSE
	COMMENT
	EOF
)

type Tokenizer struct {
	LookaheadToken int
	LookaheadLit   string
	Source         *bufrr.Reader
	CurrentCh      rune
	NextCh         rune
	Eof            bool
	AlmostEof      bool
}

var mostRecentFileTokenizer *Tokenizer
var mostRecentlyUsedFile *os.File

func NewTokenizer(scanner *bufrr.Reader) *Tokenizer {
	t := &Tokenizer{Source: scanner}
	t.Advance()
	t.ConsumeToken()
	return t
}

func NewTokenizerFromString(src string) *Tokenizer {
	return NewTokenizer(bufrr.NewReader(strings.NewReader(src)))
}

func NewTokenizerFromFile(src *os.File) *Tokenizer {
	if mostRecentlyUsedFile == src {
		return mostRecentFileTokenizer
	} else {
		t := NewTokenizer(bufrr.NewReader(src))
		mostRecentFileTokenizer = t
		mostRecentlyUsedFile = src
		return t
	}
}

func (self *Tokenizer) Advance() {
	var err error
	self.CurrentCh, _, err = self.Source.ReadRune()
	if err == io.EOF || self.CurrentCh == -1 {
		self.Eof = true
		self.NextCh = 0
	} else {
		self.NextCh, _, err = self.Source.ReadRune()
		if err == io.EOF || self.NextCh == -1 {
			self.AlmostEof = true
		} else {
			self.Source.UnreadRune()
		}
	}
}

func (self *Tokenizer) NextToken() (token int, lit string) {
	return self.LookaheadToken, self.LookaheadLit
}

func (self *Tokenizer) isSymbolCharacter(ch rune) bool {
	return unicode.IsGraphic(ch) && !unicode.IsSpace(ch) && !strings.ContainsRune("();\"'`|[]{}#,", ch)
}

func (self *Tokenizer) readSymbol() (token int, lit string) {
	buffer := make([]rune, 0, 1)
	for !self.isEof() && self.isSymbolCharacter(self.CurrentCh) {
		buffer = append(buffer, self.CurrentCh)
		self.Advance()
	}
	return SYMBOL, string(buffer)
}

func isHexChar(ch rune) bool {
	switch ch {
	case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
		return true
	case 'a', 'b', 'c', 'd', 'e', 'f', 'A', 'B', 'C', 'D', 'E', 'F':
		return true
	default:
		return false
	}
}

func isBinaryChar(ch rune) bool {
	switch ch {
	case '0', '1':
		return true
	default:
		return false
	}
}

func (self *Tokenizer) readHexNumber() (token int, lit string) {
	buffer := make([]rune, 0, 1)
	for !self.isEof() {
		ch := rune(self.CurrentCh)
		if isHexChar(ch) {
			buffer = append(buffer, self.CurrentCh)
			self.Advance()
		} else {
			break
		}
	}

	return HEXNUMBER, string(buffer)
}

func (self *Tokenizer) readBinaryNumber() (token int, lit string) {
	buffer := make([]rune, 0, 1)
	for !self.isEof() {
		ch := rune(self.CurrentCh)
		if isBinaryChar(ch) {
			buffer = append(buffer, self.CurrentCh)
			self.Advance()
		} else {
			break
		}
	}

	return BINARYNUMBER, string(buffer)
}

func (self *Tokenizer) readNumber() (token int, lit string) {
	buffer := make([]rune, 0, 1)
	isFloat := false
	sawDecimal := false
	firstChar := true
	for !self.isEof() {
		ch := rune(self.CurrentCh)
		if ch == '.' && !sawDecimal {
			isFloat = true
			sawDecimal = true
			buffer = append(buffer, self.CurrentCh)
			self.Advance()
		} else if firstChar && ch == '-' {
			buffer = append(buffer, self.CurrentCh)
			self.Advance()
		} else if unicode.IsNumber(ch) {
			buffer = append(buffer, self.CurrentCh)
			self.Advance()
		} else {
			break
		}
		firstChar = false
	}

	lit = string(buffer)
	if isFloat {
		token = FLOAT
	} else {
		token = NUMBER
	}
	return
}

func (self *Tokenizer) readString() (token int, lit string) {
	buffer := make([]rune, 0, 10)
	self.Advance()
	for !self.isEof() && rune(self.CurrentCh) != '"' {
		if rune(self.CurrentCh) == '\\' {
			self.Advance()
			if rune(self.CurrentCh) == 'n' {
				buffer = append(buffer, '\n')
			} else {
				buffer = append(buffer, rune(self.CurrentCh))
			}
			self.Advance()
			continue
		}
		buffer = append(buffer, rune(self.CurrentCh))
		self.Advance()
	}
	if self.isEof() {
		return EOF, ""
	}
	self.Advance()
	return STRING, string(buffer)
}

func (self *Tokenizer) isEof() bool {
	return self.Eof
}

func (self *Tokenizer) isAlmostEof() bool {
	return self.AlmostEof
}

func (self *Tokenizer) readNextToken() (token int, lit string) {
	if self.isEof() {
		return EOF, ""
	}
	for unicode.IsSpace(self.CurrentCh) {
		self.Advance()
		if self.isEof() {
			return EOF, ""
		}
	}

	if self.CurrentCh == '0' && self.NextCh == 'x' {
		self.Advance()
		self.Advance()
		return self.readHexNumber()
	} else if unicode.IsNumber(self.CurrentCh) {
		return self.readNumber()
	} else if self.CurrentCh == '-' && unicode.IsNumber(self.NextCh) {
		return self.readNumber()
	} else if self.CurrentCh == '"' {
		return self.readString()
	} else if self.CurrentCh == '\'' {
		self.Advance()
		return QUOTE, "'"
	} else if self.CurrentCh == '`' {
		self.Advance()
		return BACKQUOTE, "`"
	} else if self.CurrentCh == ',' && self.NextCh == '@' {
		self.Advance()
		self.Advance()
		return COMMAAT, ",@"
	} else if self.CurrentCh == ',' {
		self.Advance()
		return COMMA, ","
	} else if self.CurrentCh == '(' {
		self.Advance()
		return LPAREN, "("
	} else if self.CurrentCh == ')' {
		self.Advance()
		return RPAREN, ")"
	} else if self.CurrentCh == '[' {
		self.Advance()
		return LBRACKET, "["
	} else if self.CurrentCh == ']' {
		self.Advance()
		return RBRACKET, "]"
	} else if self.CurrentCh == '{' {
		self.Advance()
		return LBRACE, "{"
	} else if self.CurrentCh == '}' {
		self.Advance()
		return RBRACE, "}"
	} else if self.CurrentCh == '.' && self.NextCh == ' ' {
		self.Advance()
		return PERIOD, "."
	} else if self.isSymbolCharacter(self.CurrentCh) {
		return self.readSymbol()
	} else if self.CurrentCh == '#' {
		self.Advance()
		if self.CurrentCh == 't' {
			self.Advance()
			return TRUE, "#t"
		} else if self.CurrentCh == 'f' {
			self.Advance()
			return FALSE, "#f"
		} else if self.CurrentCh == 'x' {
			self.Advance()
			return self.readHexNumber()
		} else if self.CurrentCh == 'b' {
			self.Advance()
			return self.readBinaryNumber()
		} else {
			return ILLEGAL, fmt.Sprintf("#%c", self.NextCh)
		}
	} else if self.CurrentCh == ';' {
		buffer := make([]rune, 0, 1)
		for {
			if self.isEof() {
				return COMMENT, string(buffer)
			} else if self.CurrentCh == '\n' {
				return COMMENT, string(buffer)
			}
			buffer = append(buffer, self.CurrentCh)
			self.Advance()
		}
	} else {
		self.Advance()
		return ILLEGAL, fmt.Sprintf("%d", self.CurrentCh)
	}
}

func (self *Tokenizer) ConsumeToken() {
	self.LookaheadToken, self.LookaheadLit = self.readNextToken()
	if self.LookaheadToken == COMMENT { // skip comments
		self.ConsumeToken()
	}
}
