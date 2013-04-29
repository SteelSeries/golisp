// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file impliments the parser

package golisp

func arrayToList(sexprs []*Data) *Data {
    head := EmptyCons()
    lastCell := head
    for _, element := range sexprs {
        newCell := Cons(element, nil)
        lastCell.Cdr = newCell
        lastCell = newCell
    }
    return head.Cdr
}

func arrayToListWithTail(sexprs []*Data, tail *Data) *Data {
    head := EmptyCons()
    lastCell := head
    for _, element := range sexprs {
        newCell := Cons(element, nil)
        lastCell.Cdr = newCell
        lastCell = newCell
    }
    lastCell.Cdr = tail
    return head.Cdr
}
