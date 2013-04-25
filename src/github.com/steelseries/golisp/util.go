// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file impliments the parser

package golisp

func arrayToList(sexprs []Expression) Expression {
    head := EmptyCons()
    lastCell := head
    for _, element := range sexprs {
        newCell := ConsCell{element, Nil}
        lastCell.Cdr = newCell
        lastCell = newCell
    }
    return head.Cdr
}
