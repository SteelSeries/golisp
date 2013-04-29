// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file inmpliments data elements
package golisp

// Cxr

func Car(d *Data) *Data {
    if d == nil {
        return nil
    }

    if d.Type == ConsCellType {
        return d.Car
    }

    return nil
}

func Cdr(d *Data) *Data {
    if d == nil {
        return nil
    }

    if d.Type == ConsCellType {
        return d.Cdr
    }

    return nil
}

// Cxxr

func Caar(d *Data) *Data {
    return Car(Car(d))
}

func Cadr(d *Data) *Data {
    return Car(Cdr(d))
}

func Cdar(d *Data) *Data {
    return Cdr(Car(d))
}

func Cddr(d *Data) *Data {
    return Cdr(Cdr(d))
}

// Cxxxr

func Caaar(d *Data) *Data {
    return Car(Car(Car(d)))
}

func Caadr(d *Data) *Data {
    return Car(Car(Cdr(d)))
}

func Cadar(d *Data) *Data {
    return Car(Cdr(Car(d)))
}

func Caddr(d *Data) *Data {
    return Car(Cdr(Cdr(d)))
}

func Cdaar(d *Data) *Data {
    return Cdr(Car(Car(d)))
}

func Cdadr(d *Data) *Data {
    return Cdr(Car(Cdr(d)))
}

func Cddar(d *Data) *Data {
    return Cdr(Cdr(Car(d)))
}

func Cdddr(d *Data) *Data {
    return Cdr(Cdr(Cdr(d)))
}

// Cxxxxr

func Caaaar(d *Data) *Data {
    return Car(Car(Car(Car(d))))
}

func Caaadr(d *Data) *Data {
    return Car(Car(Car(Cdr(d))))
}

func Caadar(d *Data) *Data {
    return Car(Car(Cdr(Car(d))))
}

func Caaddr(d *Data) *Data {
    return Car(Car(Cdr(Cdr(d))))
}

func Cadaar(d *Data) *Data {
    return Car(Cdr(Car(Car(d))))
}

func Cadadr(d *Data) *Data {
    return Car(Cdr(Car(Cdr(d))))
}

func Caddar(d *Data) *Data {
    return Car(Cdr(Cdr(Car(d))))
}

func Cadddr(d *Data) *Data {
    return Car(Cdr(Cdr(Cdr(d))))
}

func Cdaaar(d *Data) *Data {
    return Cdr(Car(Car(Car(d))))
}

func Cdaadr(d *Data) *Data {
    return Cdr(Car(Car(Cdr(d))))
}

func Cdadar(d *Data) *Data {
    return Cdr(Car(Cdr(Car(d))))
}

func Cdaddr(d *Data) *Data {
    return Cdr(Car(Cdr(Cdr(d))))
}

func Cddaar(d *Data) *Data {
    return Cdr(Cdr(Car(Car(d))))
}

func Cddadr(d *Data) *Data {
    return Cdr(Cdr(Car(Cdr(d))))
}

func Cdddar(d *Data) *Data {
    return Cdr(Cdr(Cdr(Car(d))))
}

func Cddddr(d *Data) *Data {
    return Cdr(Cdr(Cdr(Cdr(d))))
}
