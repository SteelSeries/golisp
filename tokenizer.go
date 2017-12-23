// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpreter for embedding in a go program for scripting.
// This file implements the tokenizer.

package golisp

import (
	"fmt"
	"io"
	"os"
	"strings"
	"unicode"

	"github.com/SteelSeries/bufrr"
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

func (t *Tokenizer) Advance() {
	var err error
	t.CurrentCh, _, err = t.Source.ReadRune()
	if err == io.EOF || t.CurrentCh == -1 {
		t.Eof = true
		t.NextCh = 0
	} else {
		t.NextCh, _, err = t.Source.ReadRune()
		if err == io.EOF || t.NextCh == -1 {
			t.AlmostEof = true
		} else {
			t.Source.UnreadRune()
		}
	}
}

func (t *Tokenizer) NextToken() (token int, lit string) {
	return t.LookaheadToken, t.LookaheadLit
}

func (t *Tokenizer) isSymbolCharacter(ch rune) bool {
	return unicode.IsGraphic(ch) && !unicode.IsSpace(ch) && !strings.ContainsRune("();\"'`|[]{}#,", ch)
}

func (t *Tokenizer) readSymbol() (token int, lit string) {
	buffer := make([]rune, 0, 1)
	for !t.isEof() && t.isSymbolCharacter(t.CurrentCh) {
		buffer = append(buffer, t.CurrentCh)
		t.Advance()
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

func (t *Tokenizer) readHexNumber() (token int, lit string) {
	buffer := make([]rune, 0, 1)
	for !t.isEof() {
		ch := rune(t.CurrentCh)
		if isHexChar(ch) {
			buffer = append(buffer, t.CurrentCh)
			t.Advance()
		} else {
			break
		}
	}

	return HEXNUMBER, string(buffer)
}

func (t *Tokenizer) readBinaryNumber() (token int, lit string) {
	buffer := make([]rune, 0, 1)
	for !t.isEof() {
		ch := rune(t.CurrentCh)
		if isBinaryChar(ch) {
			buffer = append(buffer, t.CurrentCh)
			t.Advance()
		} else {
			break
		}
	}

	return BINARYNUMBER, string(buffer)
}

func (t *Tokenizer) readNumber() (token int, lit string) {
	buffer := make([]rune, 0, 1)
	isFloat := false
	sawDecimal := false
	firstChar := true
	for !t.isEof() {
		ch := rune(t.CurrentCh)
		if ch == '.' && !sawDecimal {
			isFloat = true
			sawDecimal = true
			buffer = append(buffer, t.CurrentCh)
			t.Advance()
		} else if firstChar && ch == '-' {
			buffer = append(buffer, t.CurrentCh)
			t.Advance()
		} else if unicode.IsNumber(ch) {
			buffer = append(buffer, t.CurrentCh)
			t.Advance()
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

func (t *Tokenizer) readString() (token int, lit string) {
	buffer := make([]rune, 0, 10)
	t.Advance()
	for !t.isEof() && rune(t.CurrentCh) != '"' {
		if rune(t.CurrentCh) == '\\' {
			t.Advance()
			if rune(t.CurrentCh) == 'n' {
				buffer = append(buffer, '\n')
			} else {
				buffer = append(buffer, rune(t.CurrentCh))
			}
			t.Advance()
			continue
		}
		buffer = append(buffer, rune(t.CurrentCh))
		t.Advance()
	}
	if t.isEof() {
		return EOF, ""
	}
	t.Advance()
	return STRING, string(buffer)
}

func (t *Tokenizer) isEof() bool {
	return t.Eof
}

func (t *Tokenizer) isAlmostEof() bool {
	return t.AlmostEof
}

func (t *Tokenizer) readNextToken() (token int, lit string) {
	if t.isEof() {
		return EOF, ""
	}
	for unicode.IsSpace(t.CurrentCh) {
		t.Advance()
		if t.isEof() {
			return EOF, ""
		}
	}

	if t.CurrentCh == '0' && t.NextCh == 'x' {
		t.Advance()
		t.Advance()
		return t.readHexNumber()
	} else if unicode.IsNumber(t.CurrentCh) {
		return t.readNumber()
	} else if t.CurrentCh == '-' && unicode.IsNumber(t.NextCh) {
		return t.readNumber()
	} else if t.CurrentCh == '"' {
		return t.readString()
	} else if t.CurrentCh == '\'' {
		t.Advance()
		return QUOTE, "'"
	} else if t.CurrentCh == '`' {
		t.Advance()
		return BACKQUOTE, "`"
	} else if t.CurrentCh == ',' && t.NextCh == '@' {
		t.Advance()
		t.Advance()
		return COMMAAT, ",@"
	} else if t.CurrentCh == ',' {
		t.Advance()
		return COMMA, ","
	} else if t.CurrentCh == '(' {
		t.Advance()
		return LPAREN, "("
	} else if t.CurrentCh == ')' {
		t.Advance()
		return RPAREN, ")"
	} else if t.CurrentCh == '[' {
		t.Advance()
		return LBRACKET, "["
	} else if t.CurrentCh == ']' {
		t.Advance()
		return RBRACKET, "]"
	} else if t.CurrentCh == '{' {
		t.Advance()
		return LBRACE, "{"
	} else if t.CurrentCh == '}' {
		t.Advance()
		return RBRACE, "}"
	} else if t.CurrentCh == '.' && t.NextCh == ' ' {
		t.Advance()
		return PERIOD, "."
	} else if t.isSymbolCharacter(t.CurrentCh) {
		return t.readSymbol()
	} else if t.CurrentCh == '#' {
		t.Advance()
		if t.CurrentCh == 't' {
			t.Advance()
			return TRUE, "#t"
		} else if t.CurrentCh == 'f' {
			t.Advance()
			return FALSE, "#f"
		} else if t.CurrentCh == 'x' {
			t.Advance()
			return t.readHexNumber()
		} else if t.CurrentCh == 'b' {
			t.Advance()
			return t.readBinaryNumber()
		} else {
			return ILLEGAL, fmt.Sprintf("#%c", t.NextCh)
		}
	} else if t.CurrentCh == ';' {
		buffer := make([]rune, 0, 1)
		for {
			if t.isEof() {
				return COMMENT, string(buffer)
			} else if t.CurrentCh == '\n' {
				return COMMENT, string(buffer)
			}
			buffer = append(buffer, t.CurrentCh)
			t.Advance()
		}
	} else {
		t.Advance()
		return ILLEGAL, fmt.Sprintf("%d", t.CurrentCh)
	}
}

func (t *Tokenizer) ConsumeToken() {
	t.LookaheadToken, t.LookaheadLit = t.readNextToken()
	if t.LookaheadToken == COMMENT { // skip comments
		t.ConsumeToken()
	}
}
