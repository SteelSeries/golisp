// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file implements data elements.

package golisp

import (
	"errors"
	"fmt"
	"strings"
	"unsafe"
)

const (
	ConsCellType = iota
	AlistType
	AlistCellType
	IntegerType
	FloatType
	BooleanType
	StringType
	SymbolType
	FunctionType
	MacroType
	PrimitiveType
	ObjectType
	FrameType
)

type Data struct {
	Type    int                // data type
	Car     *Data              // ConsCellType & AlistType
	Cdr     *Data              // ConsCellType & AlistType
	String  string             // StringType & SymbolType
	Integer int64              // IntegerType & BooleanType
	Float   float32            // FloatType
	Func    *Function          // FunctionType
	Mac     *Macro             // MacroType
	Prim    *PrimitiveFunction // PrimitiveType
	Frame   *FrameMap          // FrameType
	ObjType string             // ObjectType
	Obj     unsafe.Pointer     // ObjectType
}

// Boolean constants

var True *Data = &Data{Type: BooleanType, Integer: 1}
var False *Data = &Data{Type: BooleanType, Integer: 0}

// Debug support

var EvalDepth int = 0
var DebugSingleStep bool = false
var DebugCurrentFrame *SymbolTableFrame = nil
var DebugEvalInDebugRepl bool = false
var DebugErrorEnv *SymbolTableFrame = nil
var DebugOnError bool = false
var IsInteractive bool = false
var DebugReturnValue *Data = nil

func TypeOf(d *Data) int {
	return d.Type
}

func TypeName(t int) string {
	switch t {
	case ConsCellType:
		return "List"
	case AlistType:
		return "Association List"
	case AlistCellType:
		return "Association List Cell"
	case IntegerType:
		return "Integer"
	case FloatType:
		return "Float"
	case BooleanType:
		return "Boolean"
	case StringType:
		return "String"
	case SymbolType:
		return "Symbol"
	case FunctionType:
		return "Function"
	case MacroType:
		return "Macro"
	case PrimitiveType:
		return "Primitive"
	case FrameType:
		return "Frame"
	case ObjectType:
		return "Go Object"
	default:
		return "Unknown"
	}
}

func NilP(d *Data) bool {
	if d == nil {
		return true
	}
	if (PairP(d) || AlistP(d) || DottedPairP(d)) && Car(d) == nil && Cdr(d) == nil {
		return true
	}
	return false
}

func NotNilP(d *Data) bool {
	return !NilP(d)
}

func PairP(d *Data) bool {
	return d == nil || TypeOf(d) == ConsCellType
}

func ListP(d *Data) bool {
	return PairP(d) || AlistP(d)
}

func DottedPairP(d *Data) bool {
	return d == nil || TypeOf(d) == AlistCellType
}

func AlistP(d *Data) bool {
	return d == nil || TypeOf(d) == AlistType
}

func BooleanP(d *Data) bool {
	return d != nil && TypeOf(d) == BooleanType
}

func SymbolP(d *Data) bool {
	return d != nil && TypeOf(d) == SymbolType
}

func NakedP(d *Data) bool {
	return d != nil && TypeOf(d) == SymbolType && strings.HasSuffix(d.String, ":")
}

func StringP(d *Data) bool {
	return d != nil && TypeOf(d) == StringType
}

func IntegerP(d *Data) bool {
	return d != nil && TypeOf(d) == IntegerType
}

func FloatP(d *Data) bool {
	return d != nil && TypeOf(d) == FloatType
}

func NumberP(d *Data) bool {
	return IntegerP(d) || FloatP(d)
}

func ObjectP(d *Data) bool {
	return d != nil && TypeOf(d) == ObjectType
}

func FunctionP(d *Data) bool {
	return d != nil && (TypeOf(d) == FunctionType || TypeOf(d) == PrimitiveType)
}

func MacroP(d *Data) bool {
	return d != nil && TypeOf(d) == MacroType
}

func FrameP(d *Data) bool {
	return d != nil && TypeOf(d) == FrameType
}

func Cons(car *Data, cdr *Data) *Data {
	return &Data{Type: ConsCellType, Car: car, Cdr: cdr, String: "", Integer: 0, Func: nil, Prim: nil}
}

func AppendBang(l *Data, value *Data) *Data {
	if NilP(l) {
		return Cons(value, nil)
	}

	var c *Data
	for c = l; NotNilP(c.Cdr); c = Cdr(c) {
	}

	c.Cdr = Cons(value, nil)

	return l
}

func AppendBangList(l *Data, otherList *Data) *Data {
	if NilP(l) {
		return otherList
	}

	var c *Data
	for c = l; NotNilP(c.Cdr); c = Cdr(c) {
	}

	c.Cdr = otherList

	return l
}

func Append(l *Data, value *Data) *Data {
	if NilP(l) {
		return Cons(value, nil)
	}

	var newList = Copy(l)
	var c *Data
	for c = newList; NotNilP(c.Cdr); c = Cdr(c) {
	}

	c.Cdr = Cons(value, nil)

	return newList
}

func AppendList(l *Data, otherList *Data) *Data {
	if NilP(l) {
		return otherList
	}

	var newList = Copy(l)
	var c *Data
	for c = newList; NotNilP(c.Cdr); c = Cdr(c) {
	}

	c.Cdr = otherList

	return newList
}

func Acons(car *Data, cdr *Data, alist *Data) *Data {
	pair, _ := Assoc(car, alist)
	if NilP(pair) {
		cell := &Data{Type: AlistCellType, Car: car, Cdr: cdr, String: "", Integer: 0, Func: nil, Prim: nil}
		return &Data{Type: AlistType, Car: cell, Cdr: alist, String: "", Integer: 0, Func: nil, Prim: nil}
	} else {
		pair.Cdr = cdr
		return alist
	}
}

func Alist(d *Data) *Data {
	if NilP(d) {
		return nil
	}

	if PairP(d) {
		headPair := Car(d)
		return Acons(Car(headPair), Cdr(headPair), Alist(Cdr(d)))
	}

	return d
}

func InternalMakeList(c ...*Data) *Data {
	return ArrayToList(c)
}

func EmptyCons() *Data {
	return Cons(nil, nil)
}

func FrameWithValue(m *FrameMap) *Data {
	return &Data{Type: FrameType, Frame: m}
}

// func EmptyFrame() *Data {
// 	return &Data{Type: FrameType, Frame: &make(FrameMap)}
// }

func IntegerWithValue(n int64) *Data {
	return &Data{Type: IntegerType, Integer: n}
}

func FloatWithValue(n float32) *Data {
	return &Data{Type: FloatType, Float: n}
}

func BooleanWithValue(b bool) *Data {
	if b {
		return True
	} else {
		return False
	}
}

func StringWithValue(s string) *Data {
	return &Data{Type: StringType, String: s}
}

func SymbolWithName(s string) *Data {
	return &Data{Type: SymbolType, String: s}
}

func NakedSymbolWithName(s string) *Data {
	return &Data{Type: SymbolType, String: fmt.Sprintf("%s:", s)}
}

func NakedSymbolFrom(d *Data) *Data {
	return &Data{Type: SymbolType, String: fmt.Sprintf("%s:", String(d))}
}

func FunctionWithNameParamsBodyAndParent(name string, params *Data, body *Data, parentEnv *SymbolTableFrame) *Data {
	return &Data{Type: FunctionType, Func: MakeFunction(name, params, body, parentEnv)}
}

func MacroWithNameParamsBodyAndParent(name string, params *Data, body *Data, parentEnv *SymbolTableFrame) *Data {
	return &Data{Type: MacroType, Mac: MakeMacro(name, params, body, parentEnv)}
}

func PrimitiveWithNameAndFunc(name string, f *PrimitiveFunction) *Data {
	return &Data{Type: PrimitiveType, Prim: f}
}

func ObjectWithTypeAndValue(typeName string, o unsafe.Pointer) *Data {
	return &Data{Type: ObjectType, ObjType: typeName, Obj: o}
}

func IntegerValue(d *Data) int64 {
	if d == nil {
		return 0
	}

	if IntegerP(d) {
		return d.Integer
	}

	if FloatP(d) {
		return int64(d.Float)
	}

	return 0
}

func FloatValue(d *Data) float32 {
	if d == nil {
		return 0
	}

	if FloatP(d) {
		return d.Float
	}

	if IntegerP(d) {
		return float32(d.Integer)
	}

	return 0
}

func StringValue(d *Data) string {
	if d == nil {
		return ""
	}

	if StringP(d) || SymbolP(d) {
		return d.String
	}

	return ""
}

func BooleanValue(d *Data) bool {
	if d == nil {
		return false
	}

	if BooleanP(d) {
		return d.Integer != 0
	}

	return true
}

func FrameValue(d *Data) *FrameMap {
	if d == nil {
		return nil
	}

	if FrameP(d) {
		return d.Frame
	}

	return nil
}

func TypeOfObject(d *Data) (oType string) {
	if d == nil {
		return
	}

	if ObjectP(d) {
		return d.ObjType
	}

	return
}

func ObjectValue(d *Data) (p unsafe.Pointer) {
	if d == nil {
		return
	}

	if ObjectP(d) {
		return d.Obj
	}

	return
}

func Length(d *Data) int {
	if d == nil {
		return 0
	}

	if ListP(d) || AlistP(d) {
		return 1 + Length(d.Cdr)
	}

	if FrameP(d) {
		return len(*d.Frame)
	}

	return 0
}

func Reverse(d *Data) (result *Data) {
	if d == nil {
		return nil
	}

	if !ListP(d) {
		return d
	}

	var l *Data = nil
	for c := d; NotNilP(c); c = Cdr(c) {
		l = Cons(Car(c), l)
	}

	return l
}

func Flatten(d *Data) (result *Data, err error) {
	if d == nil {
		return nil, nil
	}

	if !ListP(d) {
		return d, nil
	}

	var l []*Data = make([]*Data, 0, 10)
	for c := d; NotNilP(c); c = Cdr(c) {
		if ListP(Car(c)) {
			for i := Car(c); NotNilP(i); i = Cdr(i) {
				l = append(l, Car(i))
			}
		} else {
			l = append(l, Car(c))
		}
	}

	return ArrayToList(l), nil
}

func RecursiveFlatten(d *Data) (result *Data, err error) {
	if d == nil {
		return nil, nil
	}

	if !ListP(d) {
		return d, nil
	}

	var l []*Data = make([]*Data, 0, 10)
	var elem *Data
	for c := d; NotNilP(c); c = Cdr(c) {
		if ListP(Car(c)) {
			elem, err = RecursiveFlatten(Car(c))
			if err != nil {
				return
			}
			for i := elem; NotNilP(i); i = Cdr(i) {
				l = append(l, Car(i))
			}
		} else {
			l = append(l, Car(c))
		}
	}

	return ArrayToList(l), nil
}

func QuoteIt(value *Data) (result *Data) {
	return InternalMakeList(SymbolWithName("quote"), value)
}

func QuoteAll(d *Data) (result *Data) {
	var l []*Data = make([]*Data, 0, 10)
	for c := d; NotNilP(c); c = Cdr(c) {
		l = append(l, QuoteIt(Car(c)))
	}
	return ArrayToList(l)
}

func Assoc(key *Data, alist *Data) (result *Data, err error) {
	for c := alist; NotNilP(c); c = Cdr(c) {
		pair := Car(c)
		if !DottedPairP(pair) && !PairP(pair) {
			err = errors.New("An alist MUST be made of pairs.")
			return
		}
		if IsEqual(Car(pair), key) {
			result = pair
			return
		}
	}
	return
}

func Dissoc(key *Data, alist *Data) (result *Data, err error) {
	var newList *Data = nil
	for c := alist; NotNilP(c); c = Cdr(c) {
		pair := Car(c)
		if !DottedPairP(pair) && !PairP(pair) {
			err = errors.New("An alist MUST be made of pairs.")
			return
		}
		if !IsEqual(Car(pair), key) {
			newList = Acons(Car(pair), Cdr(pair), newList)
		}
	}
	return newList, nil
}

func Copy(d *Data) *Data {
	if d == nil {
		return d
	}

	switch d.Type {
	case AlistType:
		{
			alist := Acons(Copy(Caar(d)), Copy(Cdar(d)), nil)
			for c := Cdr(d); NotNilP(c); c = Cdr(c) {
				alist = Acons(Copy(Caar(c)), Copy(Cdar(c)), alist)
			}
			return alist
		}
	case ConsCellType:
		{
			if NilP(d) {
				return d
			}

			return Cons(Copy(Car(d)), Copy(Cdr(d)))
		}
	case FrameType:
		{
			m := make(FrameMap)
			for k, v := range *d.Frame {
				m[k] = Copy(v)
			}
			return FrameWithValue(&m)
		}
	}

	return d
}

func IsEqual(d *Data, o *Data) bool {
	if d == o {
		return true
	}

	if d == nil || o == nil {
		return false
	}

	if AlistP(d) {
		if !AlistP(o) && !ListP(o) {
			return false
		}
	} else if DottedPairP(d) {
		if !PairP(o) && !DottedPairP(o) {
			return false
		}
	} else if TypeOf(o) != TypeOf(d) {
		return false
	}

	if AlistP(d) {
		if Length(d) != Length(o) {
			return false
		}
		for c := d; NotNilP(c); c = Cdr(c) {
			otherPair, err := Assoc(Caar(c), o)
			if err != nil || NilP(otherPair) || !IsEqual(Cdar(c), Cdr(otherPair)) {
				return false
			}
		}
		return true
	}

	if DottedPairP(d) {
		return IsEqual(Car(d), Car(o)) && IsEqual(Cdr(d), Cdr(o))
	}

	if ListP(d) {
		if Length(d) != Length(o) {
			return false
		}
		for a1, a2 := d, o; NotNilP(a1); a1, a2 = Cdr(a1), Cdr(a2) {
			if !IsEqual(Car(a1), Car(a2)) {
				return false
			}
		}
		return true
	}

	if FrameP(d) {
		if len(*d.Frame) != len(*o.Frame) {
			return false
		}
		for k, v := range *d.Frame {
			if !IsEqual(v, (*o.Frame)[k]) {
				return false
			}
		}
		return true
	}

	return *d == *o
}

func escapeQuotes(str string) string {
	buffer := make([]rune, 0, 10)
	for _, ch := range str {
		if rune(ch) == '"' {
			buffer = append(buffer, '\\')
		}
		buffer = append(buffer, rune(ch))
	}
	return string(buffer)
}

func String(d *Data) string {
	if d == nil {
		return "()"
	}

	switch d.Type {
	case ConsCellType:
		{
			if NilP(Car(d)) && NilP(Cdr(d)) {
				return "()"
			}
			var c *Data = d

			contents := make([]string, 0, Length(d))
			for NotNilP(c) && PairP(c) {
				contents = append(contents, String(Car(c)))
				c = Cdr(c)
			}
			if c == nil {
				if SymbolP(Car(d)) && StringValue(Car(d)) == "quote" {
					if len(contents) == 1 {
						return fmt.Sprintf("'()")
					} else {
						return fmt.Sprintf("'%s", contents[1])
					}
				} else {
					return fmt.Sprintf("(%s)", strings.Join(contents, " "))
				}
			} else {
				return fmt.Sprintf("(%s . %s)", strings.Join(contents, " "), String(c))
			}
		}
	case AlistType:
		{
			if NilP(Car(d)) && NilP(Cdr(d)) {
				return "()"
			}
			contents := make([]string, 0, Length(d))
			for c := d; NotNilP(c); c = Cdr(c) {
				contents = append(contents, String(Car(c)))
			}
			return fmt.Sprintf("(%s)", strings.Join(contents, " "))
		}
	case AlistCellType:
		return fmt.Sprintf("(%s . %s)", String(Car(d)), String(Cdr(d)))
	case IntegerType:
		return fmt.Sprintf("%d", d.Integer)
	case FloatType:
		{
			raw := fmt.Sprintf("%g", d.Float)
			if strings.ContainsRune(raw, '.') {
				return raw
			}
			return fmt.Sprintf("%s.0", raw)
		}
	case BooleanType:
		if d.Integer == 0 {
			return "#f"
		} else {
			return "#t"
		}
	case StringType:
		return fmt.Sprintf(`"%s"`, escapeQuotes(d.String))
	case SymbolType:
		return d.String
	case FunctionType:
		return fmt.Sprintf("<function: %s>", d.Func.Name)
	case MacroType:
		return fmt.Sprintf("<macro: %s>", d.Mac.Name)
	case PrimitiveType:
		return d.Prim.String()
	case ObjectType:
		if d.ObjType == "[]byte" {
			bytes := (*[]byte)(d.Obj)
			contents := make([]string, 0, len(*bytes))
			for _, b := range *bytes {
				contents = append(contents, fmt.Sprintf("%d", b))
			}
			return fmt.Sprintf("[%s]", strings.Join(contents, " "))
		} else {
			return fmt.Sprintf("<opaque Go object of type %s : 0x%x>", d.ObjType, (*uint64)(d.Obj))
		}
	case FrameType:
		pairs := make([]string, 0, len(*d.Frame))
		for key, val := range *d.Frame {
			var valString string
			if FrameP(val) {
				valString = "{...}"
			} else {
				valString = String(val)
			}
			pairs = append(pairs, fmt.Sprintf("%s %s", key, valString))
		}
		return fmt.Sprintf("{%s}", strings.Join(pairs, " "))
	}

	return ""
}

func PrintString(d *Data) string {
	if StringP(d) {
		return d.String
	} else {
		return String(d)
	}
}

func postProcessFrameShortcuts(d *Data) *Data {
	key := Car(d)
	frame := Cadr(d)
	value := Caddr(d)

	if !SymbolP(key) {
		return d
	}

	s := StringValue(key)
	switch {
	case strings.HasSuffix(s, ":"):
		return InternalMakeList(SymbolWithName("get-slot"), frame, key)
	case strings.HasSuffix(s, ":!"):
		return InternalMakeList(SymbolWithName("set-slot!"), frame, SymbolWithName(strings.TrimSuffix(s, "!")), value)
	case strings.HasSuffix(s, ":?"):
		return InternalMakeList(SymbolWithName("has-slot?"), frame, SymbolWithName(strings.TrimSuffix(s, "?")))
	default:
		return d
	}
}

func logEval(d *Data) {
	if DebugTrace && !DebugEvalInDebugRepl {
		fmt.Printf("%2d: %*sEvaling: %s\n", EvalDepth, EvalDepth, "", String(d))
		EvalDepth += 1
	}
}

func logApply(function *Data, args *Data) {
	if DebugTrace && !DebugEvalInDebugRepl {
		fmt.Printf("%2d: %*sApplying: %s to %s\n", EvalDepth, EvalDepth, "", String(function), String(args))
		EvalDepth += 1
	}
}

func logResult(result *Data) {
	if DebugTrace && !DebugEvalInDebugRepl {
		if EvalDepth > 0 {
			EvalDepth -= 1
		}
		fmt.Printf("%2d: %*s-------> %s\n", EvalDepth, EvalDepth, "", String(result))
	}
}

func Eval(d *Data, env *SymbolTableFrame) (result *Data, err error) {
	if !DebugEvalInDebugRepl && d.Type == ConsCellType {
		env.CurrentCode = fmt.Sprintf("Eval %s", String(d))
	}

	logEval(d)

	if DebugSingleStep {
		DebugSingleStep = false
		DebugRepl(env)
	}

	if DebugCurrentFrame != nil && env == DebugCurrentFrame.Parent {
		DebugCurrentFrame = nil
		DebugRepl(env)
	}

	if d != nil {
		switch d.Type {
		case ConsCellType:
			{

				d = postProcessFrameShortcuts(d)

				var function *Data
				function, err = Eval(Car(d), env)
				if err != nil {
					return
				}
				if function == nil {
					err = errors.New(fmt.Sprintf("Nil when function or macro expected for %s.", String(Car(d))))
					return
				}

				if !DebugEvalInDebugRepl && TypeOf(function) == FunctionType && function.Func.DebugOnEntry {
					DebugRepl(env)
				}

				args := Cdr(d)
				result, err = Apply(function, args, env)
				if err != nil {
					err = errors.New(fmt.Sprintf("\nEvaling %s. %s", String(d), err))
					return
				} else if DebugReturnValue != nil {
					result = DebugReturnValue
					DebugReturnValue = nil
				}
			}
		case SymbolType:
			if NakedP(d) {
				result = d
			} else {
				result = env.ValueOf(d)
			}
		default:
			result = d
		}
	}
	logResult(result)
	return result, nil
}

func formatApply(function *Data, args *Data) string {
	var fname string

	if function == nil {
		return "Trying to apply nil!"
	}

	switch function.Type {
	case FunctionType:
		fname = function.Func.Name
	case MacroType:
		fname = function.Mac.Name
	case PrimitiveType:
		fname = function.Prim.Name
	}
	return fmt.Sprintf("Apply %s to %s", fname, String(args))
}

func Apply(function *Data, args *Data, env *SymbolTableFrame) (result *Data, err error) {
	logApply(function, args)
	if !DebugEvalInDebugRepl && len(env.CurrentCode) == 0 {
		env.CurrentCode = formatApply(function, args)
	}

	if function == nil {
		err = errors.New("Nil when function expected.")
		return
	}
	switch function.Type {
	case FunctionType:
		result, err = function.Func.Apply(args, env)
	case MacroType:
		result, err = function.Mac.Apply(args, env)
	case PrimitiveType:
		result, err = function.Prim.Apply(args, env)
	}

	logResult(result)
	return
}

func ApplyWithoutEval(function *Data, args *Data, env *SymbolTableFrame) (result *Data, err error) {
	logApply(function, args)
	if !DebugEvalInDebugRepl && len(env.CurrentCode) == 0 {
		env.CurrentCode = formatApply(function, args)
	}

	if function == nil {
		err = errors.New("Nil when function or macro expected.")
		return
	}
	switch function.Type {
	case FunctionType:
		result, err = function.Func.ApplyWithoutEval(args, env)
	case MacroType:
		result, err = function.Mac.ApplyWithoutEval(args, env)
	case PrimitiveType:
		result, err = function.Prim.ApplyWithoutEval(args, env)
	}

	logResult(result)
	return
}
