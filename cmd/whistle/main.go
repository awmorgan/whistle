package main

import (
	"fmt"
	"strings"
)

func main() {
	l := Lisp{}
	l.process = &process{pid: "<pid1>"}
	p := Proc{isBuiltin: true, sexpression: sexpression{value: BuiltinProc(add)}}
	l.Env = &Env{dict: map[string]SExpression{"+": p}}
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
	dict  map[string]SExpression
	outer *Env
}

func (e *Env) find(s string) (*Env, bool) {
	if _, ok := e.dict[s]; ok {
		return e, true
	}
	return e.outer.find(s)
}

func (e *Env) replace(s string, sexp SExpression) bool {
	outer, _ := e.find(s)
	outer.dict[s] = sexp
	return true
}

func newEnv(params Pair, args []SExpression, outer *Env) *Env {
	m := map[string]SExpression{}
	i := 0
	for params != NewPair(nil, nil) {
		m[params.pcar.AsString()] = args[i]
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
			if e.IsString() {
				ed, _ := env.find(e.AsString())
				return ed.dict[e.AsString()], nil
			}
			return e, nil
		}
		ep := e.(Pair)
		car := ep.pcar
		if car.IsString() {
			s := car.AsString()
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
				sym := ep.cadr().AsString()
				exp := ep.caddr()
				evalled, _ := p.evalEnv(env, exp)
				env.dict[sym] = evalled
				return NewAtom(false), nil
			case "set!":
				sym := ep.cadr().AsString()
				exp := ep.caddr()
				evalled, _ := p.evalEnv(env, exp)
				env.replace(sym, evalled)
				return NewAtom(false), nil
			case "define-syntax":
				keyword := ep.cadr().AsString()
				transformer := ep.caddr().(Pair)
				macromap[keyword] = syntaxRules(keyword, transformer)
				return NewAtom(false), nil
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
			return proc.value.(BuiltinProc)(p, env, args)
		}
		defproc := proc.value.(DefinedProc)
		env, e = newEnv(defproc.params, args, defproc.env), defproc.body
	}
}

func (s sexpression) IsString() bool {
	return s.isAtom && s.isString
}

func (s sexpression) IsAtom() bool {
	return s.isExpression && s.isAtom
}

func (s sexpression) IsPair() bool {
	return s.isExpression && !s.isAtom
}

func (s sexpression) AsString() string {
	return s.value.(string)
}

func (s sexpression) AsFloat64() float64 {
	return s.value.(float64)
}

func Newstring(s string) Atom {
	a := NewAtom(s)
	a.isString = true
	return a
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
	IsString() bool
	IsAtom() bool
	IsPair() bool
	AsString() string
	AsFloat64() float64
}

type sexpression struct {
	isExpression bool
	isAtom       bool
	isString     bool
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

type DefinedProc struct {
	params Pair
	body   SExpression
	env    *Env
}

type BuiltinProc = func(*process, *Env, []SExpression) (SExpression, error)

type process struct {
	pid string
}

func add(p *process, env *Env, args []SExpression) (SExpression, error) {
	return NewAtom(args[0].AsFloat64() + args[1].AsFloat64()), nil
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
	if !p.pcar.IsString() {
		return p, false
	}
	s := p.pcar.AsString()
	tf, ok := macromap[s]
	if !ok {
		return p, false
	}
	return tf(p), true
}

func syntaxRules(keyword string, sr Pair) transformer {
	literals := []string{keyword, "lambda", "define", "begin"}
	for _, e := range cons2list(sr.cadr().(Pair)) {
		literals = append(literals, e.AsString())
	}
	clauses := []clause{}
	for _, c := range cons2list(sr.cddr().(Pair)) {
		cp := c.(Pair)
		s := map[string]string{}
		e := map[string]int{}
		p := analysePattern(literals, cp.pcar, s, e)
		t := analyseTemplate(literals, cp.cadr(), s, e)
		clauses = append(clauses, clause{pattern: p, template: t, ellipsis: e})
	}
	return func(p Pair) SExpression {
		substitutions := map[string]SExpression{}
		fmt.Printf("in closure: clause[0].pattern.isList: %v\n", clauses[0].pattern.isList)
		unify(clauses[0].pattern, p, substitutions)
		return substituteTemplate(clauses[0].template, substitutions, clauses[0].ellipsis)
	}
}

type clause struct {
	pattern  pattern
	template pattern
	ellipsis map[string]int
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

func gensym() string {
	symbolCounter += 1
	return string(fmt.Sprintf("gensym%d", symbolCounter))
}

func analyse(literals []string, p SExpression, gensyms map[string]string, build bool) pattern {
	if p.IsString() {
		sym := p.AsString()
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
			return pattern{isVariable: true, content: Newstring(newsym)}
		}
		newsym, ok := gensyms[sym]
		if !ok {
			return pattern{isVariable: true, content: p}
		}
		return pattern{isVariable: true, content: Newstring(newsym)}
	}
	listContent := []pattern{}
	list := cons2list(p.(Pair))
	for i := 0; i < len(list); i++ {
		pi := analyse(literals, list[i], gensyms, build)
		if i != len(list)-1 {
			sexprj := list[i+1]
			if sexprj.IsString() && sexprj.AsString() == ellipsis {
				pi.hasEllipsis = true
				i += 1
			}
		}
		listContent = append(listContent, pi)
	}
	return pattern{isList: true, listContent: listContent}
}

func analysePattern(literals []string, p SExpression, gensyms map[string]string, ellipsis map[string]int) pattern {
	pattern := analyse(literals, p, gensyms, true)
	analyseEllipsis(pattern, ellipsis, 0)
	return pattern
}

func analyseTemplate(literals []string, t SExpression, gensyms map[string]string, ellipsis map[string]int) pattern {
	pattern := analyse(literals, t, gensyms, false)
	return pattern
}

func analyseEllipsis(p pattern, e map[string]int, depth int) {
	if p.isVariable {
		if depth == 0 && !p.hasEllipsis {
			return
		}
		ps := p.content.AsString()
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

func unify(p pattern, q SExpression, s map[string]SExpression) bool {
	return unifyWithEllipsis(p, q, s, []int{})
}

func unifyWithEllipsis(p pattern, q SExpression, s map[string]SExpression, depth []int) bool {
	if p.isUnderscore {
		return true
	}
	if p.isVariable {
		ps := p.content.AsString()
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

func substituteTemplate(template pattern, substitutions map[string]SExpression, ellipsis map[string]int) SExpression {
	sexpr, _ := substituteTemplateWithEllipsis(template, substitutions, ellipsis, []int{})
	return sexpr
}

func substituteTemplateWithEllipsis(template pattern, substitutions map[string]SExpression, e map[string]int, depth []int) (SExpression, bool) {
	if template.isLiteral {
		return template.content, false
	}
	if template.isVariable {
		ss := template.content.AsString()
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
		ss = template.content.AsString()
		s, ok = substitutions[ss]
		if ok {
			return s, false
		}
		newsym := Newstring(gensym())
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
		return NewAtom(0.0)
	} else if token == "1" {
		return NewAtom(1.0)
	}
	if token[0] == '\'' {
		quote, _, _ := readFromTokens([]string{"(", "quote", token[1:], ")"})
		return quote
	}
	return Newstring(token)
}
