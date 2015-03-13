// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file implements the parser.

package golisp

import (
	"errors"
	"fmt"
	"io/ioutil"
	"unsafe"
)

func makeInteger(str string) (n *Data, err error) {
	var i int64
	_, err = fmt.Sscanf(str, "%d", &i)
	if err != nil {
		return
	}
	n = IntegerWithValue(i)
	return
}

func makeHexInteger(str string) (n *Data, err error) {
	var i int64
	_, err = fmt.Sscanf(str, "%v", &i)
	if err != nil {
		return
	}
	n = IntegerWithValue(i)
	return
}

func makeFloat(str string) (n *Data, err error) {
	var i float32
	_, err = fmt.Sscanf(str, "%f", &i)
	if err != nil {
		return
	}
	n = FloatWithValue(i)
	return
}

func makeString(str string) (s *Data, err error) {
	s = StringWithValue(str)
	return
}

func makeSymbol(str string) (s *Data, err error) {
	s = Intern(str)
	return
}

func parseConsCell(s *Tokenizer) (sexpr *Data, eof bool, err error) {
	tok, _ := s.NextToken()
	if tok == RPAREN {
		s.ConsumeToken()
		sexpr = nil
		return
	}

	var car *Data
	var cdr *Data
	cells := make([]*Data, 0, 10)
	for tok != RPAREN {
		if tok == PERIOD {
			s.ConsumeToken()
			cdr, eof, err = parseExpression(s)
			if eof || err != nil {
				return
			}
			tok, _ = s.NextToken()
			if tok != RPAREN {
				err = errors.New("Expected ')'")
				return
			}
			s.ConsumeToken()
			sexpr = ArrayToListWithTail(cells, cdr)
			return
		} else {
			car, eof, err = parseExpression(s)
			if eof {
				err = errors.New("Unexpected EOF (expected closing parenthesis)")
				return
			}
			if err != nil {
				return
			}
			cells = append(cells, car)
		}
		tok, _ = s.NextToken()
	}

	s.ConsumeToken()
	sexpr = ArrayToList(cells)
	return
}

func allIntegers(data []*Data) bool {
	for _, n := range data {
		if !IntegerP(n) {
			return false
		}
	}
	return true
}

func listToBytearray(cells []*Data) *Data {
	bytes := make([]byte, 0, len(cells))
	for _, cell := range cells {
		b := IntegerValue(cell)
		bytes = append(bytes, byte(b))
	}
	return ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&bytes))
}

func parseBytearray(s *Tokenizer) (sexpr *Data, eof bool, err error) {
	tok, _ := s.NextToken()
	if tok == RBRACKET {
		s.ConsumeToken()
		bytes := make([]byte, 0)
		sexpr = ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&bytes))
		return
	}

	var element *Data
	cells := make([]*Data, 0, 10)
	for tok != RBRACKET {
		element, eof, err = parseExpression(s)
		if eof {
			err = errors.New("Unexpected EOF (expected closing bracket)")
			return
		}
		if err != nil {
			return
		}
		if IntegerP(element) && IntegerValue(element) > 255 {
			err = errors.New(fmt.Sprintf("Numeric literals in a bytearray must be bytes. Encountered %s.", String(element)))
			return
		}
		if !IntegerP(element) && !SymbolP(element) && !ListP(element) {
			err = errors.New(fmt.Sprintf("Bytearray elements must be numbers, symbols, or lists (function calls). Encountered %s.", String(element)))
			return
		}
		cells = append(cells, element)
		tok, _ = s.NextToken()
	}

	s.ConsumeToken()
	if allIntegers(cells) {
		sexpr = listToBytearray(cells)
	} else {
		sexpr = InternalMakeList(Intern("list-to-bytearray"), QuoteIt(ArrayToList(cells)))
	}
	return
}

func parseFrame(s *Tokenizer) (sexpr *Data, eof bool, err error) {
	tok, _ := s.NextToken()
	if tok == RBRACKET {
		s.ConsumeToken()
		f := make(FrameMap)
		sexpr = FrameWithValue(&f)
		return
	}

	var element *Data
	cells := make([]*Data, 0, 10)
	for tok != RBRACE {
		element, eof, err = parseExpression(s)
		if eof {
			err = errors.New("Unexpected EOF (expected closing brace)")
			return
		}
		if err != nil {
			return
		}
		cells = append(cells, element)
		tok, _ = s.NextToken()
	}

	s.ConsumeToken()
	sexpr = Cons(Intern("make-frame"), ArrayToList(cells))
	return
}

func parseExpression(s *Tokenizer) (sexpr *Data, eof bool, err error) {
	for {
		tok, lit := s.NextToken()
		switch tok {
		case EOF:
			eof = true
			err = nil
			return
		case COMMENT:
			s.ConsumeToken()
			break
		case NUMBER:
			s.ConsumeToken()
			sexpr, err = makeInteger(lit)
			return
		case HEXNUMBER:
			s.ConsumeToken()
			sexpr, err = makeHexInteger(lit)
			return
		case FLOAT:
			s.ConsumeToken()
			sexpr, err = makeFloat(lit)
			return
		case STRING:
			s.ConsumeToken()
			sexpr, err = makeString(lit)
			return
		case LPAREN:
			s.ConsumeToken()
			sexpr, eof, err = parseConsCell(s)
			return
		case LBRACKET:
			s.ConsumeToken()
			sexpr, eof, err = parseBytearray(s)
			return
		case LBRACE:
			s.ConsumeToken()
			sexpr, eof, err = parseFrame(s)
			return
		case SYMBOL:
			s.ConsumeToken()
			sexpr, err = makeSymbol(lit)
			return
		case FALSE:
			s.ConsumeToken()
			sexpr = LispFalse
			return
		case TRUE:
			s.ConsumeToken()
			sexpr = LispTrue
			return
		case QUOTE:
			s.ConsumeToken()
			sexpr, eof, err = parseExpression(s)
			if sexpr != nil {
				sexpr = Cons(Intern("quote"), Cons(sexpr, nil))
			}
			return
		case BACKQUOTE:
			s.ConsumeToken()
			sexpr, eof, err = parseExpression(s)
			if sexpr != nil {
				sexpr = Cons(Intern("quasiquote"), Cons(sexpr, nil))
			}
			return
		case COMMA:
			s.ConsumeToken()
			sexpr, eof, err = parseExpression(s)
			if sexpr != nil {
				sexpr = Cons(Intern("unquote"), Cons(sexpr, nil))
			}
			return
		case COMMAAT:
			s.ConsumeToken()
			sexpr, eof, err = parseExpression(s)
			if sexpr != nil {
				sexpr = Cons(Intern("unquote-splicing"), Cons(sexpr, nil))
			}
			return
		case ILLEGAL:
			err = errors.New(fmt.Sprintf("Illegal character: %s", lit))
			return
		default:
			s.ConsumeToken()
			sexpr, err = makeSymbol(lit)
			return
		}
	}
}

func Parse(src string) (sexpr *Data, err error) {
	s := NewTokenizer(src)
	sexpr, _, err = parseExpression(s)
	return
}

func ParseAll(src string) (result []*Data, err error) {
	s := NewTokenizer(src)
	var sexpr *Data
	var eof bool
	for {
		sexpr, eof, err = parseExpression(s)
		if err != nil || eof {
			break
		}
		result = append(result, sexpr)
	}
	return
}

func ReadFile(filename string) (s string, err error) {
	contents, err := ioutil.ReadFile(filename)
	if err != nil {
		return
	}

	s = *(*string)(unsafe.Pointer(&contents))
	return
}

func ProcessFile(filename string) (result *Data, err error) {
	src, err := ReadFile(filename)
	if err != nil {
		return
	}
	result, err = ParseAndEvalAll(src)
	return
}

func ParseAndEvalAll(src string) (result *Data, err error) {
	s := NewTokenizer(src)
	var sexpr *Data
	var eof bool
	for {
		sexpr, eof, err = parseExpression(s)
		if err != nil {
			return
		}
		if eof {
			return
		}
		if NilP(sexpr) {
			return
		}
		result, err = Eval(sexpr, Global)
		if err != nil {
			return
		}
	}
	return
}

func ParseAndEval(src string) (result *Data, err error) {
	s := NewTokenizer(src)
	var sexpr *Data
	sexpr, _, err = parseExpression(s)
	if err != nil {
		return
	}
	if NilP(sexpr) {
		return
	}
	result, err = Eval(sexpr, Global)
	if err != nil {
		return
	}
	return
}

func ParseAndEvalAllInEnvironment(src string, env *SymbolTableFrame) (result *Data, err error) {
	s := NewTokenizer(src)
	var sexpr *Data
	var eof bool
	for {
		sexpr, eof, err = parseExpression(s)
		if err != nil {
			return
		}
		if eof {
			return
		}
		if NilP(sexpr) {
			return
		}
		result, err = Eval(sexpr, env)
		if err != nil {
			return
		}
	}
	return
}

func ParseAndEvalInEnvironment(src string, env *SymbolTableFrame) (result *Data, err error) {
	s := NewTokenizer(src)
	var sexpr *Data
	sexpr, _, err = parseExpression(s)
	if err != nil {
		return
	}
	if NilP(sexpr) {
		return
	}
	result, err = Eval(sexpr, env)
	if err != nil {
		return
	}
	return
}