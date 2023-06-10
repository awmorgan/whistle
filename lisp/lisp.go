package lisp

import (
	"fmt"
	"strconv"
)

type Lisp struct {
	process *process
	Env     *Env
}

func New() Lisp {
	p := newProcess()
	env := GlobalEnv()
	return Lisp{p, env}
}

func (l Lisp) EvalExpr(e SExpression) (SExpression, error) {
	return l.process.evalEnv(l.Env, e)
}

// Load a string of lisp code/data into the environment.
func (l Lisp) Load(data string) error {
	sexprs, _ := Multiparse(data)
	for _, def := range sexprs {
		l.EvalExpr(def)
	}
	return nil
}

type Env struct {
	dict  map[Symbol]SExpression
	outer *Env
}

func (e *Env) find(s Symbol) (*Env, bool) {
	if _, ok := e.dict[s]; ok {
		return e, true
	}
	return e.outer.find(s)
}

func (e *Env) replace(s Symbol, sexp SExpression) bool {
	outer, _ := e.find(s)
	outer.dict[s] = sexp
	return true
}

func newEnv(params Pair, args []SExpression, outer *Env) *Env {
	m := map[Symbol]SExpression{}
	i := 0
	for params != empty {
		m[params.car().AsSymbol()] = args[i]
		params = params.cdr().AsPair()
		i++
	}
	return &Env{dict: m, outer: outer}
}

func (p *process) evalEnv(env *Env, e SExpression) (SExpression, error) {
Loop:
	for {
		if e.IsPair() {
			ex, ok := expandMacro(e.AsPair())
			if ok {
				e = ex
				continue Loop
			}
		}
		if e.IsAtom() {
			if e.IsSymbol() {
				ed, _ := env.find(e.AsSymbol())
				return ed.dict[e.AsSymbol()], nil
			}
			// primitive
			return e, nil
		}
		// list, at which point car should be one of two things:
		// smth evaluating to procedure, or one of a few builtin symbols (which mark builtin procedures)
		ep := e.AsPair()
		car := ep.car()
		if car.IsSymbol() {
			s := car.AsSymbol()
			// builtin funcs that arent like other builtins:
			// they rely on their args not being evaluated first
			// their syntactic forms are checked at read-time
			switch s {
			case "begin":
				args := ep.cdr().AsPair()
				for args.cdr().AsPair() != empty {
					p.evalEnv(env, args.car())
					args = args.cdr().AsPair()
				}
				e = args.car()
				continue Loop
			case "quote":
				return ep.cadr(), nil
			case "define":
				sym := ep.cadr().AsSymbol()
				exp := ep.caddr()
				evalled, _ := p.evalEnv(env, exp)
				env.dict[sym] = evalled
				return NewPrimitive(false), nil
			case "set!":
				sym := ep.cadr().AsSymbol()
				exp := ep.caddr()
				evalled, _ := p.evalEnv(env, exp)
				// TODO: will silently fail if not found
				// check output bool if you want a proper error
				env.replace(sym, evalled)
				return NewPrimitive(false), nil
			case "define-syntax":
				keyword := ep.cadr().AsSymbol()
				transformer := ep.caddr().AsPair()
				macromap[keyword] = syntaxRules(keyword, transformer)
				return NewPrimitive(false), nil
			case "lambda":
				params := ep.cadr().AsPair()
				body := ep.caddr()
				return Proc{sexpression: sexpression{
					value: DefinedProc{
						params: params,
						body:   body,
						env:    env,
					},
				}}, nil
				// default: falls through to procedure call
			}
		}
		// procedure call
		peval, _ := p.evalEnv(env, car)
		e = peval
		proc := e.AsProcedure()
		pargs := ep.cdr().AsPair()
		args := []SExpression{}
		for pargs != empty {
			args = append(args, pargs.car())
			pargs = pargs.cdr().AsPair()
		}
		for i, arg := range args {
			evarg, _ := p.evalEnv(env, arg)
			args[i] = evarg
		}
		if proc.isBuiltin {
			return proc.builtin()(p, env, args)
		}
		defproc := proc.defined()
		env, e = newEnv(defproc.params, args, defproc.env), defproc.body
	}
}

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

type ExternalProc = func([]SExpression) (SExpression, error)
