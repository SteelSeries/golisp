// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpreter for embedding in a go program for scripting.
// This file implements data elements.

package golisp

import (
	"errors"
	"fmt"
	"math"
	"os"
	"sort"
	"strings"
	"sync/atomic"
	"unsafe"

	"gopkg.in/fatih/set.v0"
)

const (
	NilType = iota
	ConsCellType
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
	BoxedObjectType
	FrameType
	EnvironmentType
	PortType
)

type ConsCell struct {
	Car *Data
	Cdr *Data
}

type BoxedObject struct {
	ObjType string
	Obj     unsafe.Pointer
}

type Data struct {
	Type  uint8
	Value unsafe.Pointer
}

// Boolean constants

type BooleanBox struct {
	B bool
}

var bTrue = true
var bFalse = false

var LispTrue = &Data{Type: BooleanType, Value: unsafe.Pointer(&bTrue)}
var LispFalse = &Data{Type: BooleanType, Value: unsafe.Pointer(&bFalse)}

// Debug support

var EvalDepth = 0
var DebugSingleStep = false
var DebugCurrentFrame *SymbolTableFrame = nil
var DebugEvalInDebugRepl = false
var DebugErrorEnv *SymbolTableFrame = nil
var DebugOnError = false
var IsInteractive = false
var DebugReturnValue *Data = nil
var DebugOnEntry = set.New()

func TypeOf(d *Data) uint8 {
	if d == nil {
		return NilType
	} else {
		return d.Type
	}
}

func TypeName(t uint8) string {
	switch t {
	case NilType:
		return "Nil"
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
	case BoxedObjectType:
		return "Go Object"
	case EnvironmentType:
		return "Environment"
	case PortType:
		return "Port"
	default:
		return "Unknown"
	}
}

// Function has heavy traffic, try to keep it fast
func NilP(d *Data) bool {
	if d == nil {
		return true
	}

	if d.Type == ConsCellType || d.Type == AlistType || d.Type == AlistCellType {
		cell := (*ConsCell)(d.Value)

		if cell == nil || (cell.Car == nil && cell.Cdr == nil) {
			return true
		}
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
	return d != nil && TypeOf(d) == SymbolType && strings.HasSuffix(StringValue(d), ":")
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
	return d != nil && TypeOf(d) == BoxedObjectType
}

func FunctionOrPrimitiveP(d *Data) bool {
	return d != nil && (TypeOf(d) == FunctionType || TypeOf(d) == PrimitiveType)
}

func FunctionP(d *Data) bool {
	return d != nil && TypeOf(d) == FunctionType
}

func PrimitiveP(d *Data) bool {
	return d != nil && TypeOf(d) == PrimitiveType
}

func MacroP(d *Data) bool {
	return d != nil && TypeOf(d) == MacroType
}

func FrameP(d *Data) bool {
	return d != nil && TypeOf(d) == FrameType
}

func EnvironmentP(d *Data) bool {
	return d != nil && TypeOf(d) == EnvironmentType
}

func PortP(d *Data) bool {
	return d != nil && TypeOf(d) == PortType
}

func EmptyCons() *Data {
	cell := ConsCell{Car: nil, Cdr: nil}
	return &Data{Type: ConsCellType, Value: unsafe.Pointer(&cell)}
}

func Cons(car *Data, cdr *Data) *Data {
	cell := ConsCell{Car: car, Cdr: cdr}
	return &Data{Type: ConsCellType, Value: unsafe.Pointer(&cell)}
}

func AppendBang(l *Data, value *Data) *Data {
	if NilP(l) {
		return Cons(value, nil)
	}

	var c *Data
	for c = l; NotNilP(Cdr(c)); c = Cdr(c) {
	}

	((*ConsCell)(c.Value)).Cdr = Cons(value, nil)

	return l
}

func AppendBangList(l *Data, otherList *Data) *Data {
	if NilP(l) {
		return otherList
	}

	var c *Data
	for c = l; NotNilP(Cdr(c)); c = Cdr(c) {
	}

	((*ConsCell)(c.Value)).Cdr = otherList

	return l
}

func Append(l *Data, value *Data) *Data {
	if NilP(l) {
		return Cons(value, nil)
	}

	var newList = Copy(l)
	var c *Data
	for c = newList; NotNilP(Cdr(c)); c = Cdr(c) {
	}

	((*ConsCell)(c.Value)).Cdr = Cons(value, nil)

	return newList
}

func AppendList(l *Data, otherList *Data) *Data {
	if NilP(l) {
		return otherList
	}

	var newList = Copy(l)
	var c *Data
	for c = newList; NotNilP(Cdr(c)); c = Cdr(c) {
	}

	((*ConsCell)(c.Value)).Cdr = otherList

	return newList
}

func RemoveFromListBang(l *Data, item *Data) (result *Data) {
	var prev *Data
	result = l
	for cell := l; NotNilP(cell); cell = Cdr(cell) {
		if IsEqual(item, Car(cell)) {
			if NilP(prev) {
				return Cdr(cell)
			}
			((*ConsCell)(prev.Value)).Cdr = Cdr(cell)
			return
		}
		prev = cell
	}
	return
}

func Acons(car *Data, cdr *Data, alist *Data) *Data {
	pair, _ := Assoc(car, alist)
	if NilP(pair) {
		p := ConsCell{Car: car, Cdr: cdr}
		cell := Data{Type: AlistCellType, Value: unsafe.Pointer(&p)}
		conscell := ConsCell{Car: &cell, Cdr: alist}
		return &Data{Type: AlistType, Value: unsafe.Pointer(&conscell)}
	} else {
		((*ConsCell)(pair.Value)).Cdr = cdr
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

func FrameWithValue(m *FrameMap) *Data {
	return &Data{Type: FrameType, Value: unsafe.Pointer(m)}
}

// func EmptyFrame() *Data {
// 	return &Data{Type: FrameType, Frame: &make(FrameMap)}
// }

func IntegerWithValue(n int64) *Data {
	return &Data{Type: IntegerType, Value: unsafe.Pointer(&n)}
}

func FloatWithValue(n float32) *Data {
	return &Data{Type: FloatType, Value: unsafe.Pointer(&n)}
}

func BooleanWithValue(b bool) *Data {
	if b {
		return LispTrue
	} else {
		return LispFalse
	}
}

func StringWithValue(s string) *Data {
	return &Data{Type: StringType, Value: unsafe.Pointer(&s)}
}

func SetStringValue(d *Data, s string) *Data {
	if StringP(d) {
		d.Value = unsafe.Pointer(&s)
		return d
	} else {
		return nil
	}
}

func SymbolWithName(s string) *Data {
	return &Data{Type: SymbolType, Value: unsafe.Pointer(&s)}
}

func NakedSymbolWithName(s string) *Data {
	return Intern(fmt.Sprintf("%s:", s))
}

func NakedSymbolFrom(d *Data) *Data {
	return NakedSymbolWithName(StringValue(d))
}

func FunctionWithNameParamsBodyAndParent(name string, params *Data, body *Data, parentEnv *SymbolTableFrame) *Data {
	return &Data{Type: FunctionType, Value: unsafe.Pointer(MakeFunction(name, params, body, parentEnv))}
}

func MacroWithNameParamsBodyAndParent(name string, params *Data, body *Data, parentEnv *SymbolTableFrame) *Data {
	return &Data{Type: MacroType, Value: unsafe.Pointer(MakeMacro(name, params, body, parentEnv))}
}

func PrimitiveWithNameAndFunc(name string, f *PrimitiveFunction) *Data {
	return &Data{Type: PrimitiveType, Value: unsafe.Pointer(f)}
}

func ObjectWithTypeAndValue(typeName string, o unsafe.Pointer) *Data {
	bo := BoxedObject{ObjType: typeName, Obj: o}
	return &Data{Type: BoxedObjectType, Value: unsafe.Pointer(&bo)}
}

func EnvironmentWithValue(e *SymbolTableFrame) *Data {
	return &Data{Type: EnvironmentType, Value: unsafe.Pointer(e)}
}

func PortWithValue(e *os.File) *Data {
	return &Data{Type: PortType, Value: unsafe.Pointer(e)}
}

func ConsValue(d *Data) *ConsCell {
	if d == nil {
		return nil
	}

	if PairP(d) || AlistP(d) || DottedPairP(d) {
		return (*ConsCell)(d.Value)
	}

	return nil
}

// Function has heavy traffic, try to keep it fast
func Car(d *Data) *Data {
	if d == nil {
		return nil
	}

	if d.Type == ConsCellType || d.Type == AlistType || d.Type == AlistCellType {
		cell := (*ConsCell)(d.Value)

		if cell != nil {
			return cell.Car
		}
	}

	return nil
}

// Function has heavy traffic, try to keep it fast
func Cdr(d *Data) *Data {
	if d == nil {
		return nil
	}

	if d.Type == ConsCellType || d.Type == AlistType || d.Type == AlistCellType {
		cell := (*ConsCell)(d.Value)

		if cell != nil {
			return cell.Cdr
		}
	}

	return nil
}

func IntegerValue(d *Data) int64 {
	if d == nil {
		return 0
	}

	if IntegerP(d) {
		return *((*int64)(d.Value))
	}

	if FloatP(d) {
		return int64(*((*float32)(d.Value)))
	}

	return 0
}

func FloatValue(d *Data) float32 {
	if d == nil {
		return 0
	}

	if FloatP(d) {
		return *((*float32)(d.Value))
	}

	if IntegerP(d) {
		return float32(*((*int64)(d.Value)))
	}

	return 0
}

func StringValue(d *Data) string {
	if d == nil {
		return ""
	}

	if StringP(d) || SymbolP(d) {
		return *((*string)(d.Value))
	}

	return ""
}

func BooleanValue(d *Data) bool {
	if NilP(d) {
		return false
	}

	if BooleanP(d) {
		return *((*bool)(d.Value))
	}

	return true
}

func FrameValue(d *Data) *FrameMap {
	if d == nil {
		return nil
	}

	if FrameP(d) {
		return (*FrameMap)(d.Value)
	}

	return nil
}

func FunctionValue(d *Data) *Function {
	if d == nil {
		return nil
	}

	if d.Type == FunctionType {
		return (*Function)(d.Value)
	}

	return nil
}

func MacroValue(d *Data) *Macro {
	if d == nil {
		return nil
	}

	if d.Type == MacroType {
		return (*Macro)(d.Value)
	}

	return nil
}

func PrimitiveValue(d *Data) *PrimitiveFunction {
	if d == nil {
		return nil
	}

	if d.Type == PrimitiveType {
		return (*PrimitiveFunction)(d.Value)
	}

	return nil
}

func ObjectType(d *Data) (oType string) {
	if d == nil {
		return
	}

	if ObjectP(d) {
		return (*((*BoxedObject)(d.Value))).ObjType
	}

	return
}

func ObjectValue(d *Data) (p unsafe.Pointer) {
	if d == nil {
		return
	}

	if ObjectP(d) {
		return (*((*BoxedObject)(d.Value))).Obj
	}

	return
}

func BoxedObjectValue(d *Data) *BoxedObject {
	if d == nil {
		return nil
	}

	if ObjectP(d) {
		return (*BoxedObject)(d.Value)
	}

	return nil
}

func EnvironmentValue(d *Data) *SymbolTableFrame {
	if d == nil {
		return nil
	}

	if EnvironmentP(d) {
		return (*SymbolTableFrame)(d.Value)
	}

	return nil
}

func PortValue(d *Data) *os.File {
	if d == nil {
		return nil
	}

	if PortP(d) {
		return (*os.File)(d.Value)
	}

	return nil
}

// Function has heavy traffic, try to keep it fast, at least for the list/bytearray cases
func Length(d *Data) int {
	if d == nil {
		return 0
	}

	if d.Type == ConsCellType || d.Type == AlistType {
		l := 0
		for c := d; NotNilP(c); c = Cdr(c) {
			l += 1
		}
		return l
	}

	if d.Type == BoxedObjectType && ((*BoxedObject)(d.Value)).ObjType == "[]byte" {
		dBytes := *(*[]byte)(((*BoxedObject)(d.Value)).Obj)
		return len(dBytes)
	}

	if FrameP(d) {
		frame := FrameValue(d)
		frame.Mutex.RLock()
		length := len(frame.Data)
		frame.Mutex.RUnlock()
		return length
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

	var l = make([]*Data, 0, 10)
	for c := d; NotNilP(c); c = Cdr(c) {
		if ListP(Car(c)) {
			for i := Car(c); i != nil; i = Cdr(i) {
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

	var l = make([]*Data, 0, 10)
	var elem *Data
	for c := d; NotNilP(c); c = Cdr(c) {
		if ListP(Car(c)) {
			elem, err = RecursiveFlatten(Car(c))
			if err != nil {
				return
			}
			for i := elem; i != nil; i = Cdr(i) {
				l = append(l, Car(i))
			}
		} else {
			l = append(l, Car(c))
		}
	}

	return ArrayToList(l), nil
}

func QuoteIt(value *Data) (result *Data) {
	if value == nil {
		return InternalMakeList(Intern("quote"), Cons(nil, nil))
	} else {
		return InternalMakeList(Intern("quote"), value)
	}
}

func QuoteAll(d *Data) (result *Data) {
	var l = make([]*Data, 0, 10)
	if d == nil {
		l = append(l, QuoteIt(nil))
	} else {
		for c := d; c != nil; c = Cdr(c) {
			l = append(l, QuoteIt(Car(c)))
		}
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
			m := FrameMap{}
			m.Data = make(FrameMapData)
			frame := FrameValue(d)
			frame.Mutex.RLock()
			for k, v := range frame.Data {
				m.Data[k] = Copy(v)
			}
			frame.Mutex.RUnlock()
			return FrameWithValue(&m)
		}
	case BoxedObjectType:
		{
			if ObjectType(d) == "[]byte" {
				bytes := (*[]byte)(ObjectValue(d))
				copy := append([]byte{}, *bytes...)
				return ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&copy))
			}
		}
	}

	return d
}

func IsEqual(d *Data, o *Data) bool {
	if d == o && !FloatP(d) {
		return true
	}

	if NilP(d) && NilP(o) {
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
		frameD := FrameValue(d)
		frameO := FrameValue(o)
		frameD.Mutex.RLock()
		frameO.Mutex.RLock()
		if len(frameD.Data) != len(frameO.Data) {
			frameO.Mutex.RUnlock()
			frameD.Mutex.RUnlock()
			return false
		}
		for k, v := range frameD.Data {
			if !IsEqual(v, frameO.Data[k]) {
				frameO.Mutex.RUnlock()
				frameD.Mutex.RUnlock()
				return false
			}
		}
		frameO.Mutex.RUnlock()
		frameD.Mutex.RUnlock()
		return true
	}

	// special case for byte arrays
	if ObjectP(d) && ObjectType(d) == "[]byte" && ObjectType(o) == "[]byte" {
		dBytes := *(*[]byte)(ObjectValue(d))
		oBytes := *(*[]byte)(ObjectValue(o))

		if len(dBytes) != len(oBytes) {
			return false
		} else {
			for i := 0; i < len(dBytes); i++ {
				if dBytes[i] != oBytes[i] {
					return false
				}
			}
		}
		return true
	}

	switch TypeOf(d) {
	case IntegerType:
		return IntegerValue(d) == IntegerValue(o)
	case FloatType:
		return FloatValue(d) == FloatValue(o)
	case BooleanType:
		return BooleanValue(d) == BooleanValue(o)
	case StringType, SymbolType: // check symbols not generated using intern (aka: gensym and gensym-naked)
		return StringValue(d) == StringValue(o)
	case FunctionType:
		return FunctionValue(d) == FunctionValue(o)
	case MacroType:
		return MacroValue(d) == MacroValue(o)
	case PrimitiveType:
		return PrimitiveValue(d) == PrimitiveValue(o)
	case BoxedObjectType:
		return (ObjectType(d) == ObjectType(o)) && (ObjectValue(d) == ObjectValue(o))
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
			if NilP(d) {
				return "()"
			}
			var c = d

			contents := make([]string, 0, Length(d))
			for NotNilP(c) && PairP(c) {
				contents = append(contents, String(Car(c)))
				c = Cdr(c)
			}
			if NilP(c) {
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
			if NilP(d) {
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
		return fmt.Sprintf("%d", IntegerValue(d))
	case FloatType:
		{
			v := FloatValue(d)
			if math.IsInf(float64(v), 0) {
				sign := "+"
				if math.Signbit(float64(v)) {
					sign = "-"
				}
				return fmt.Sprintf("%sinf", sign)
			}
			if math.IsNaN(float64(v)) {
				return "nan"
			}
			raw := fmt.Sprintf("%g", FloatValue(d))
			if strings.ContainsRune(raw, '.') {
				return raw
			}
			return fmt.Sprintf("%s.0", raw)
		}
	case BooleanType:
		if BooleanValue(d) {
			return "#t"
		} else {
			return "#f"
		}
	case StringType:
		return fmt.Sprintf(`"%s"`, escapeQuotes(StringValue(d)))
	case SymbolType:
		return StringValue(d)
	case FunctionType:
		return fmt.Sprintf("<function: %s>", FunctionValue(d).Name)
	case MacroType:
		return fmt.Sprintf("<macro: %s>", MacroValue(d).Name)
	case PrimitiveType:
		return fmt.Sprintf("<prim: %s>", PrimitiveValue(d).Name)
	case BoxedObjectType:
		if ObjectType(d) == "[]byte" {
			bytes := (*[]byte)(ObjectValue(d))
			contents := make([]string, 0, len(*bytes))
			for _, b := range *bytes {
				contents = append(contents, fmt.Sprintf("%d", b))
			}
			return fmt.Sprintf("[%s]", strings.Join(contents, " "))
		} else {
			return fmt.Sprintf("<opaque Go object of type %s : 0x%x>", ObjectType(d), (*uint64)(ObjectValue(d)))
		}
	case FrameType:
		frame := FrameValue(d)
		frame.Mutex.RLock()
		keys := make([]string, 0, len(frame.Data))
		for key := range frame.Data {
			keys = append(keys, key)
		}
		sort.Strings(keys)

		pairs := make([]string, 0, len(frame.Data))
		for _, key := range keys {
			val := frame.Data[key]
			var valString = String(val)
			pairs = append(pairs, fmt.Sprintf("%s %s", key, valString))
		}
		frame.Mutex.RUnlock()
		return fmt.Sprintf("{%s}", strings.Join(pairs, " "))
	case EnvironmentType:
		return fmt.Sprintf("<environment: %s>", EnvironmentValue(d).Name)
	case PortType:
		return fmt.Sprintf("<port: %s>", PortValue(d).Name())
	}

	return ""
}

func PrintString(d *Data) string {
	if StringP(d) {
		return StringValue(d)
	} else {
		return String(d)
	}
}

func postProcessShortcuts(d *Data) *Data {
	symbolObj := Car(d)

	if !SymbolP(symbolObj) {
		return d
	}

	pseudoFunction := StringValue(symbolObj)

	switch {
	// channel shortcuts
	case strings.HasPrefix(pseudoFunction, "<-"):
		return AppendBangList(InternalMakeList(Intern("channel-read"), Intern(strings.TrimPrefix(pseudoFunction, "<-"))), Cdr(d))
	case strings.HasSuffix(pseudoFunction, "<-"):
		return AppendBangList(InternalMakeList(Intern("channel-write"), Intern(strings.TrimSuffix(pseudoFunction, "<-"))), Cdr(d))

		// frame shortcuts
	case strings.HasSuffix(pseudoFunction, ":"):
		return AppendBangList(InternalMakeList(Intern("get-slot"), Cadr(d), Car(d)), Cddr(d))
	case strings.HasSuffix(pseudoFunction, ":!"):
		return AppendBangList(InternalMakeList(Intern("set-slot!"), Cadr(d), Intern(strings.TrimSuffix(pseudoFunction, "!")), Caddr(d)), Cdddr(d))
	case strings.HasSuffix(pseudoFunction, ":?"):
		return AppendBangList(InternalMakeList(Intern("has-slot?"), Cadr(d), Intern(strings.TrimSuffix(pseudoFunction, "?"))), Cddr(d))
	case strings.HasSuffix(pseudoFunction, ":>"):
		return AppendBangList(InternalMakeList(Intern("send"), Cadr(d), Intern(strings.TrimSuffix(pseudoFunction, ">"))), Cddr(d))
	case strings.HasSuffix(pseudoFunction, ":^"):
		return AppendBangList(InternalMakeList(Intern("send-super"), Intern(strings.TrimSuffix(pseudoFunction, "^"))), Cdr(d))
	default:
		return d
	}
}

func printDashes(indent int) {
	for i := indent; i > 0; i -= 1 {
		fmt.Print("-")
	}
}

func logEval(d *Data, env *SymbolTableFrame) {
	if LispTrace && !DebugEvalInDebugRepl {
		depth := env.Depth()
		fmt.Printf("%3d: ", depth)
		printDashes(depth)
		fmt.Printf("> %s\n", String(d))
		EvalDepth += 1
	}
}

func logResult(result *Data, env *SymbolTableFrame) {
	if LispTrace && !DebugEvalInDebugRepl {
		depth := env.Depth()
		fmt.Printf("%3d: <", depth)
		printDashes(depth)
		fmt.Printf(" %s\n", String(result))
	}
}

func evalHelper(d *Data, env *SymbolTableFrame, needFunction bool) (result *Data, err error) {
	if IsInteractive && !DebugEvalInDebugRepl {
		env.CurrentCode.PushFront(fmt.Sprintf("Eval %s", String(d)))
	}

	logEval(d, env)

	if DebugSingleStep {
		DebugSingleStep = false
		DebugRepl(env)
	}

	if DebugCurrentFrame != nil && env == DebugCurrentFrame.Previous {
		DebugCurrentFrame = nil
		DebugRepl(env)
	}

	if d != nil {
		switch d.Type {
		case ConsCellType:
			{
				d = postProcessShortcuts(d)

				// catch empty cons cell
				if NilP(d) {
					return EmptyCons(), nil
				}

				var function *Data
				function, err = evalHelper(Car(d), env, true)

				if err != nil {
					return
				}
				if NilP(function) {
					err = errors.New(fmt.Sprintf("Nil when function or macro expected for %s.", String(Car(d))))
					return
				}

				if !DebugSingleStep && TypeOf(function) == FunctionType && DebugOnEntry.Has(FunctionValue(function).Name) {
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
				result = env.ValueOfWithFunctionSlotCheck(d, needFunction)
			}
		default:
			result = d
		}
	}
	logResult(result, env)
	if IsInteractive && !DebugEvalInDebugRepl && env.CurrentCode.Len() > 0 {
		env.CurrentCode.Remove(env.CurrentCode.Front())
	}
	return result, nil
}

func Eval(d *Data, env *SymbolTableFrame) (result *Data, err error) {
	return evalHelper(d, env, false)
}

func Apply(function *Data, args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if NilP(function) {
		err = errors.New("Nil when function expected.")
		return
	}
	switch function.Type {
	case FunctionType:
		if atomic.LoadInt32(&FunctionValue(function).SlotFunction) == 1 && env.HasFrame() {
			result, err = FunctionValue(function).ApplyWithFrame(args, env, env.Frame)
		} else {
			result, err = FunctionValue(function).Apply(args, env)
		}
	case MacroType:
		result, err = MacroValue(function).Apply(args, env)
	case PrimitiveType:
		result, err = PrimitiveValue(function).Apply(args, env)
	default:
		err = errors.New(fmt.Sprintf("%s when function or macro expected for %s.", TypeName(TypeOf(function)), String(function)))
		return
	}

	return
}

func ApplyWithoutEval(function *Data, args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if function == nil {
		err = errors.New("Nil when function or macro expected.")
		return
	}
	switch function.Type {
	case FunctionType:
		if atomic.LoadInt32(&FunctionValue(function).SlotFunction) == 1 && env.HasFrame() {
			result, err = FunctionValue(function).ApplyWithoutEvalWithFrame(args, env, env.Frame)
		} else {
			result, err = FunctionValue(function).ApplyWithoutEval(args, env)
		}
		//		result, err = FunctionValue(function).ApplyWithoutEval(args, env)
	case MacroType:
		result, err = MacroValue(function).ApplyWithoutEval(args, env)
	case PrimitiveType:
		result, err = PrimitiveValue(function).ApplyWithoutEval(args, env)
	default:
		err = errors.New(fmt.Sprintf("%s when function or macro expected for %s.", TypeName(TypeOf(function)), String(function)))
		return
	}

	return
}
