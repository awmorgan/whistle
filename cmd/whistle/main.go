package main

import (
	"fmt"
	"strings"
)

func main() {
	l := Lisp{}
	l.process = &process{pid: "<pid1>"}
	l.Env = &Env{dict: map[Symbol]SExpression{"+": builtinFunc(add)}}
	sexprs, _ := Multiparse(datalog)
	for _, def := range sexprs {
		l.process.evalEnv(l.Env, def)
	}

	sexpressions, _ := Multiparse("(define a (dl_record 'vertex))")
	for _, e := range sexpressions {
		l.process.evalEnv(l.Env, e)
	}
}

var datalog string = `
(define dl_counter 0)
(define dl_nextID (lambda () (begin
   (set! dl_counter (+ dl_counter 1))
   dl_counter)))

(define dl_assert (lambda (entity attr value) (begin
  (hashmap-set! dl_edb (list entity attr value) #t)
  (dl_update_indices (list entity attr value)))))

(define-syntax dl_record
   (syntax-rules (list let dl_nextID dl_assert)
     ((_ type (attr value) ...) (let ((id (dl_nextID)))
       (dl_assert id (list type attr) value) ... id))))
`

type Lisp struct {
	process *process
	Env     *Env
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
	for params != NewPair(nil, nil) {
		m[params.pcar.AsSymbol()] = args[i]
		params = params.pcdr.(Pair)
		i++
	}
	return &Env{dict: m, outer: outer}
}

func (p *process) evalEnv(env *Env, e SExpression) (SExpression, error) {
Loop:
	for {
		if e.IsPair() {
			ex, ok := expandMacro(e.(Pair))
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
			return e, nil
		}
		ep := e.(Pair)
		car := ep.pcar
		if car.IsSymbol() {
			s := car.AsSymbol()
			switch s {
			case "begin":
				args := ep.pcdr.(Pair)
				for args.pcdr.(Pair) != NewPair(nil, nil) {
					p.evalEnv(env, args.pcar)
					args = args.pcdr.(Pair)
				}
				e = args.pcar
				continue Loop
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
				env.replace(sym, evalled)
				return NewPrimitive(false), nil
			case "define-syntax":
				keyword := ep.cadr().AsSymbol()
				transformer := ep.caddr().(Pair)
				macromap[keyword] = syntaxRules(keyword, transformer)
				return NewPrimitive(false), nil
			case "lambda":
				params := ep.cadr().(Pair)
				body := ep.caddr()
				return Proc{sexpression: sexpression{
					value: DefinedProc{
						params: params,
						body:   body,
						env:    env,
					},
				}}, nil
			}
		}
		peval, _ := p.evalEnv(env, car)
		e = peval
		proc := e.(Proc)
		pargs := ep.pcdr.(Pair)
		args := []SExpression{}
		for pargs != NewPair(nil, nil) {
			args = append(args, pargs.pcar)
			pargs = pargs.pcdr.(Pair)
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

func (s sexpression) IsSymbol() bool {
	return s.isAtom && s.isSymbol
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

type SExpression interface {
	IsSymbol() bool
	IsAtom() bool
	IsPair() bool
	AsSymbol() Symbol
	AsNumber() Number
}

type sexpression struct {
	isExpression bool
	isAtom       bool
	isSymbol     bool
	value        any
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

func (p Pair) cadr() SExpression {
	return p.pcdr.(Pair).pcar
}

func (p Pair) caddr() SExpression {
	return p.pcdr.(Pair).pcdr.(Pair).pcar
}

func (p Pair) cddr() SExpression {
	return p.pcdr.(Pair).pcdr
}

func list2cons(list ...SExpression) Pair {
	if len(list) == 0 {
		return NewPair(nil, nil)
	}
	if len(list) == 1 {
		return NewPair(list[0], NewPair(nil, nil))
	}
	cons := NewPair(nil, nil)
	for i := len(list) - 1; i >= 0; i-- {
		cons = NewPair(list[i], cons)
	}
	return cons
}

func cons2list(p Pair) []SExpression {
	list := []SExpression{}
	for p != NewPair(nil, nil) {
		list = append(list, p.pcar)
		p = p.pcdr.(Pair)
	}
	return list
}

type Proc struct {
	sexpression
	isBuiltin bool // user defined proc if false
}

func (p Proc) builtin() BuiltinProc {
	return p.value.(BuiltinProc)
}
func (p Proc) defined() DefinedProc {
	return p.value.(DefinedProc)
}

type DefinedProc struct {
	params Pair
	body   SExpression
	env    *Env
}

type BuiltinProc = func(*process, *Env, []SExpression) (SExpression, error)

type process struct {
	pid string
}

func builtinFunc(f BuiltinProc) Proc {
	return Proc{
		isBuiltin: true,
		sexpression: sexpression{
			value: f,
		},
	}
}

func add(p *process, env *Env, args []SExpression) (SExpression, error) {
	return NewPrimitive(args[0].AsNumber() + args[1].AsNumber()), nil
}

const ellipsis = "..."
const underscore = "_"

var macromap = map[string]transformer{}

func init() {
	s := mustParse(`(syntax-rules ()
                                 ((_ ((var exp) ...) body1 body2 ...)
                                   ((lambda (var ...) (begin body1 body2 ...)) exp ...)))`)
	s1 := s.(Pair)
	s2 := syntaxRules("let", s1)
	macromap["let"] = s2
}

func mustParse(program string) SExpression {
	p, _ := parse(program)
	return p
}

func parse(program string) (SExpression, error) {
	p, _, err := readFromTokens(tokenize(program))
	return p, err
}

type transformer = func(Pair) SExpression

func expandMacro(p Pair) (SExpression, bool) {
	if !p.pcar.IsSymbol() {
		return p, false
	}
	s := p.pcar.AsSymbol()
	tf, ok := macromap[s]
	if !ok {
		return p, false
	}
	return tf(p), true
}

func syntaxRules(keyword string, sr Pair) transformer {
	literals := []string{keyword, "lambda", "define", "begin"}
	for _, e := range cons2list(sr.cadr().(Pair)) {
		literals = append(literals, e.AsSymbol())
	}
	clauses := []clause{}
	for _, c := range cons2list(sr.cddr().(Pair)) {
		cp := c.(Pair)
		s := map[Symbol]Symbol{}
		e := map[Symbol]int{}
		p := analysePattern(literals, cp.pcar, s, e)
		t := analyseTemplate(literals, cp.cadr(), s, e)
		clauses = append(clauses, clause{pattern: p, template: t, ellipsis: e})
	}
	return func(p Pair) SExpression {
		for i, c := range clauses {
			substitutions := map[Symbol]SExpression{}
			fmt.Printf("in closure: clause[%d].pattern.isList: %v\n", i, c.pattern.isList)
			unify(c.pattern, p, substitutions)
			return substituteTemplate(c.template, substitutions, c.ellipsis)
		}
		return nil
	}
}

type clause struct {
	pattern  pattern
	template pattern
	ellipsis map[Symbol]int
}

type pattern struct {
	isVariable   bool
	isUnderscore bool
	isLiteral    bool
	isList       bool
	hasEllipsis  bool
	content      SExpression
	listContent  []pattern
}

var symbolCounter int

func gensym() Symbol {
	symbolCounter += 1
	return Symbol(fmt.Sprintf("gensym%d", symbolCounter))
}

func analyse(literals []string, p SExpression, gensyms map[Symbol]Symbol, build bool) pattern {
	if p.IsSymbol() {
		sym := p.AsSymbol()
		if sym == underscore {
			return pattern{isUnderscore: true}
		}
		for _, lit := range literals {
			if lit == sym {
				return pattern{isLiteral: true, content: p}
			}
		}
		if build {
			newsym := gensym()
			gensyms[sym] = newsym
			return pattern{isVariable: true, content: NewSymbol(newsym)}
		}
		newsym, ok := gensyms[sym]
		if !ok {
			return pattern{isVariable: true, content: p}
		}
		return pattern{isVariable: true, content: NewSymbol(newsym)}
	}
	listContent := []pattern{}
	list := cons2list(p.(Pair))
	for i := 0; i < len(list); i++ {
		pi := analyse(literals, list[i], gensyms, build)
		if i != len(list)-1 {
			sexprj := list[i+1]
			if sexprj.IsSymbol() && sexprj.AsSymbol() == ellipsis {
				pi.hasEllipsis = true
				i += 1
			}
		}
		listContent = append(listContent, pi)
	}
	return pattern{isList: true, listContent: listContent}
}

func analysePattern(literals []string, p SExpression, gensyms map[Symbol]Symbol, ellipsis map[Symbol]int) pattern {
	pattern := analyse(literals, p, gensyms, true)
	analyseEllipsis(pattern, ellipsis, 0)
	return pattern
}

func analyseTemplate(literals []string, t SExpression, gensyms map[Symbol]Symbol, ellipsis map[Symbol]int) pattern {
	pattern := analyse(literals, t, gensyms, false)
	return pattern
}

func analyseEllipsis(p pattern, e map[Symbol]int, depth int) {
	if p.isVariable {
		if depth == 0 && !p.hasEllipsis {
			return
		}
		ps := p.content.AsSymbol()
		if p.hasEllipsis {
			depth++
		}
		e[ps] = depth
		return
	}
	if !p.isList {
		return
	}
	newdepth := depth
	if p.hasEllipsis {
		newdepth++
	}
	for _, pp := range p.listContent {
		analyseEllipsis(pp, e, newdepth)
	}
}

func unify(p pattern, q SExpression, s map[Symbol]SExpression) bool {
	return unifyWithEllipsis(p, q, s, []int{})
}

func unifyWithEllipsis(p pattern, q SExpression, s map[Symbol]SExpression, depth []int) bool {
	if p.isUnderscore {
		return true
	}
	if p.isVariable {
		ps := p.content.AsSymbol()
		for i := 0; i < len(depth); i++ {
			ps += fmt.Sprintf("#%d", depth[i])
		}
		s[ps] = q
		return true
	}
	if !p.isList {
		panic("pattern assumed to be list")
	}
	qp := q.(Pair)
Loop:
	for _, pp := range p.listContent {
		if !pp.hasEllipsis {
			unifyWithEllipsis(pp, qp.pcar, s, depth)
			qp = qp.pcdr.(Pair)
			continue Loop
		}
		newdepth := make([]int, len(depth))
		copy(newdepth, depth)
		newdepth = append(newdepth, 0)
		for {
			if qp == NewPair(nil, nil) {
				continue Loop
			}
			unifyWithEllipsis(pp, qp.pcar, s, newdepth)
			newdepth[len(newdepth)-1] = newdepth[len(newdepth)-1] + 1
			qp = qp.pcdr.(Pair)
		}
	}
	return qp == NewPair(nil, nil)
}

func substituteTemplate(template pattern, substitutions map[Symbol]SExpression, ellipsis map[Symbol]int) SExpression {
	sexpr, _ := substituteTemplateWithEllipsis(template, substitutions, ellipsis, []int{})
	return sexpr
}

func substituteTemplateWithEllipsis(template pattern, substitutions map[Symbol]SExpression, e map[Symbol]int, depth []int) (SExpression, bool) {
	if template.isLiteral {
		return template.content, false
	}
	if template.isVariable {
		ss := template.content.AsSymbol()
		_, isEllipsis := e[ss]
		for i := 0; i < len(depth); i++ {
			ss += fmt.Sprintf("#%d", depth[i])
		}
		s, ok := substitutions[ss]
		if ok {
			return s, isEllipsis
		}
		if isEllipsis {
			return template.content, false
		}
		ss = template.content.AsSymbol()
		s, ok = substitutions[ss]
		if ok {
			return s, false
		}
		newsym := NewSymbol(gensym())
		substitutions[ss] = newsym
		return newsym, false
	}
	out := []SExpression{}
	found := false
	for _, v := range template.listContent {
		if !v.hasEllipsis {
			sexpr, ok := substituteTemplateWithEllipsis(v, substitutions, e, depth)
			found = found || ok
			out = append(out, sexpr)
			continue
		}
		newdepth := make([]int, len(depth))
		copy(newdepth, depth)
		newdepth = append(newdepth, 0)
		i := 0
		for {
			sexpr, ok := substituteTemplateWithEllipsis(v, substitutions, e, newdepth)
			if !ok {
				break
			}
			out = append(out, sexpr)
			newdepth[len(newdepth)-1] = newdepth[len(newdepth)-1] + 1
			i++
		}
		found = found || (i != 0)
	}
	return list2cons(out...), found
}

func Multiparse(file string) ([]SExpression, error) {
	tokens := tokenize(file)
	exprs := []SExpression{}
	for len(tokens) > 0 {
		e, rem, _ := readFromTokens(tokens)
		exprs = append(exprs, e)
		tokens = rem
	}
	return exprs, nil
}

func tokenize(s string) []string {
	s = strings.ReplaceAll(s, "(", " ( ")
	s = strings.ReplaceAll(s, ")", " ) ")
	return append([]string{}, strings.Fields(s)...)
}

func readFromTokens(tokens []string) (SExpression, []string, error) {
	token := tokens[0]
	tokens = tokens[1:]
	switch token {
	case "(":
		list := []SExpression{}
		for tokens[0] != ")" {
			parsed, t, _ := readFromTokens(tokens)
			tokens = t
			list = append(list, parsed)
		}
		return list2cons(list...), tokens[1:], nil
	default:
		return atom(token), tokens, nil
	}
}

func atom(token string) SExpression {
	if token == "0" {
		return NewPrimitive(0.0)
	} else if token == "1" {
		return NewPrimitive(1.0)
	}
	if token[0] == '\'' {
		quote, _, _ := readFromTokens([]string{"(", "quote", token[1:], ")"})
		return quote
	}
	return NewSymbol(token)
}
