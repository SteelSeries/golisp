// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file implements data elements.

package golisp

// Cxr

func WalkList(d *Data, path string) *Data {
	c := d
	for index := len(path) - 1; index >= 0; index-- {
		if c == nil {
			return nil
		}
		if !PairP(c) && !AlistP(c) && !DottedPairP(c) {
			return nil
		}
		switch path[index] {
		case 'a':
			c = Car(c)
		case 'd':
			c = Cdr(c)
		default:
			c = nil
		}
	}
	return c
}

func Car(d *Data) *Data {
	if d == nil {
		return nil
	}

	if PairP(d) || AlistP(d) || DottedPairP(d) {
		return d.Car
	}

	return nil
}

func Cdr(d *Data) *Data {
	if d == nil {
		return nil
	}

	if PairP(d) || AlistP(d) || DottedPairP(d) {
		return d.Cdr
	}

	return nil
}

// Cxxr

func Caar(d *Data) *Data {
	return WalkList(d, "aa")
}

func Cadr(d *Data) *Data {
	return WalkList(d, "ad")
}

func Cdar(d *Data) *Data {
	return WalkList(d, "da")
}

func Cddr(d *Data) *Data {
	return WalkList(d, "dd")
}

// nth

func Nth(d *Data, n int) *Data {
	if d == nil || n < 1 || n > Length(d) {
		return nil
	}

	var c *Data = d
	for i := n; i > 1; c, i = Cdr(c), i-1 {
	}
	return Car(c)
}

func First(d *Data) *Data {
	return Nth(d, 1)
}

func Second(d *Data) *Data {
	return Nth(d, 2)
}

func Third(d *Data) *Data {
	return Nth(d, 3)
}

func Fourth(d *Data) *Data {
	return Nth(d, 4)
}

func Fifth(d *Data) *Data {
	return Nth(d, 5)
}

func Sixth(d *Data) *Data {
	return Nth(d, 6)
}

func Seventh(d *Data) *Data {
	return Nth(d, 7)
}

func Eighth(d *Data) *Data {
	return Nth(d, 8)
}

func Ninth(d *Data) *Data {
	return Nth(d, 9)
}

func Tenth(d *Data) *Data {
	return Nth(d, 10)
}
