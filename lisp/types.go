package lisp

import (
	"fmt"
	"strconv"
)

type SExpression interface {
	IsSymbol() bool
	IsNumber() bool
	IsAtom() bool
	IsPair() bool
	AsSymbol() Symbol
	AsNumber() Number
	AsAtom() Atom
	AsPair() Pair
	AsProcedure() Proc //edure
}

type sexpression struct {
	isExpression bool
	isAtom       bool
	isSymbol     bool
	value        any
}

// NOTE: the below panics should never occur;
// main loop should guard against that

func (s sexpression) IsSymbol() bool {
	return s.isExpression && s.isAtom && s.isSymbol
}

func (s sexpression) IsNumber() bool {
	_, ok := s.value.(Number)
	return ok
}

func (s sexpression) IsAtom() bool {
	return s.isExpression && s.isAtom
}

func (s sexpression) IsPair() bool {
	return s.isExpression && !s.isAtom
}

func (s sexpression) AsSymbol() Symbol {
	return s.value.(Symbol)
}

func (s sexpression) AsNumber() Number {
	return s.value.(Number)
}

func (s sexpression) AsAtom() Atom {
	panic("not an atom")
}

func (s sexpression) AsPair() Pair {
	panic("not a pair")
}

func (s sexpression) AsProcedure() Proc {
	panic("not a procedure")
}

type Symbol = string

func NewSymbol(s string) Atom {
	a := NewAtom(s)
	a.isSymbol = true
	return a
}

type Number = float64

func NewPrimitive(v any) Atom {
	return NewAtom(v)
}

type Atom struct {
	sexpression
}

func NewAtom(v any) Atom {
	return Atom{sexpression{
		isExpression: true,
		isAtom:       true,
		value:        v,
	}}
}

func (a Atom) AsAtom() Atom {
	return a
}

func (a Atom) String() string {
	if a.IsSymbol() {
		return a.AsSymbol()
	}
	// TODO: hacked bool type into Number type here!
	if _, ok := a.value.(bool); ok {
		return ""
	}
	if s, ok := a.value.(string); ok {
		return fmt.Sprintf("%q", s)
	}
	if m, ok := a.value.(map[SExpression]SExpression); ok {
		return fmt.Sprintf("%v", m)
	}
	return strconv.FormatFloat(a.AsNumber(), 'f', -1, 64)
}

type Pair struct {
	sexpression
	pcar SExpression
	pcdr SExpression
}

func NewPair(car, cdr SExpression) Pair {
	return Pair{
		sexpression: sexpression{
			isExpression: true,
		},
		pcar: car,
		pcdr: cdr,
	}
}

func (p Pair) AsPair() Pair {
	return p
}

func (p Pair) car() SExpression {
	return p.pcar
}

func (p Pair) cdr() SExpression {
	return p.pcdr
}

func (p Pair) cadr() SExpression {
	return p.cdr().AsPair().car()
}

func (p Pair) caddr() SExpression {
	return p.cdr().AsPair().cdr().AsPair().car()
}

func (p Pair) cddr() SExpression {
	return p.cdr().AsPair().cdr()
}

var empty Pair = NewPair(nil, nil)

func list2cons(list ...SExpression) Pair {
	if len(list) == 0 {
		return empty
	}
	if len(list) == 1 {
		return NewPair(list[0], empty)
	}
	cons := empty
	for i := len(list) - 1; i >= 0; i-- {
		cons = NewPair(list[i], cons)
	}
	return cons
}

func cons2list(p Pair) []SExpression {
	list := []SExpression{}
	for p != empty {
		list = append(list, p.pcar)
		p = p.pcdr.AsPair()
	}
	return list
}

type Proc struct {
	sexpression
	isBuiltin bool // user defined proc if false
}

func (p Proc) AsProcedure() Proc {
	return p
}

func (p Proc) builtin() BuiltinProc {
	return p.value.(BuiltinProc)
}
func (p Proc) defined() DefinedProc {
	return p.value.(DefinedProc)
}

func (p Proc) String() string {
	return "#<proc>"
}

type DefinedProc struct {
	params Pair
	body   SExpression
	env    *Env
}

type BuiltinProc = func(*process, *Env, []SExpression) (SExpression, error)

// for use outside of lisp package, since process shouldnt be exported
type ExternalProc = func([]SExpression) (SExpression, error)
