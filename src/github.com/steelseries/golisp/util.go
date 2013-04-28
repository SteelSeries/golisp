// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file impliments the parser

package golisp

import "fmt"

func arrayToList(sexprs []Expression) Expression {
    fmt.Printf("arrayToList: %v\n", sexprs)

    head := EmptyCons()
    lastCell := &head
    for _, element := range sexprs {
        fmt.Printf("%v\n", element)
        newCell := Cons(element, Nil)
        lastCell.Cdr = newCell
        c := lastCell.Cdr.(ConsCell)
        lastCell = &c
        println(head.Cdr.String())
    }
    return head.Cdr
}
