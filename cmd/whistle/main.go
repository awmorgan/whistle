package main

import (
	"fmt"
	"strings"
)

type SE interface {
	IsS() bool
	IsA() bool
	IsP() bool
	AsS() string
	AsF() float64
}

type se struct {
	isExpression, isAtom, isString bool
	value                          interface{}
}

type Atom struct {
	se
}

type Pair struct {
	se
	pcar, pcdr SE
}

type Proc struct {
	se
	isBuiltin bool
}

type Lisp struct {
	process *process
	Env     *Env
}

type Env struct {
	dict  map[string]SE
	outer *Env
}

type DefinedProc struct {
	params Pair
	body   SE
	env    *Env
}

type BuiltinProc = func(*process, *Env, []SE) (SE, error)

type process struct {
	pid string
}

type transformer = func(Pair) SE

type clause struct {
	pattern, template pattern
	ellipsis          map[string]int
}

type pattern struct {
	isVariable, isUnderscore, isLiteral, isList, hasEllipsis bool
	content                                                  SE
	listContent                                              []pattern
}

func main() {
	l := Lisp{}
	l.process = &process{pid: "<pid1>"}
	p := Proc{se{value: BuiltinProc(add)}, true}
	l.Env = &Env{dict: map[string]SE{"+": p}}
	sexprs, _ := Multiparse(`
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
	`)
	for _, def := range sexprs {
		l.process.evalEnv(l.Env, def)
	}

	sexpressions, _ := Multiparse("(define a (dl_record 'vertex))")
	for _, e := range sexpressions {
		l.process.evalEnv(l.Env, e)
	}
}

func (e *Env) find(s string) (*Env, bool) {
	if _, ok := e.dict[s]; ok {
		return e, true
	}
	return e.outer.find(s)
}

func (e *Env) replace(s string, sexp SE) bool {
	outer, _ := e.find(s)
	outer.dict[s] = sexp
	return true
}

func newEnv(params Pair, args []SE, outer *Env) *Env {
	m := map[string]SE{}
	i := 0
	for params != NP(nil, nil) {
		m[params.pcar.AsS()] = args[i]
		params = params.pcdr.(Pair)
		i++
	}
	return &Env{dict: m, outer: outer}
}

func (p *process) evalEnv(env *Env, e SE) (SE, error) {
Loop:
	for {
		if e.IsP() {
			ex, ok := expandMacro(e.(Pair))
			if ok {
				e = ex
				continue Loop
			}
		}
		if e.IsA() {
			if e.IsS() {
				ed, _ := env.find(e.AsS())
				return ed.dict[e.AsS()], nil
			}
			return e, nil
		}
		ep := e.(Pair)
		car := ep.pcar
		if car.IsS() {
			s := car.AsS()
			switch s {
			case "begin":
				args := ep.pcdr.(Pair)
				for args.pcdr.(Pair) != NP(nil, nil) {
					p.evalEnv(env, args.pcar)
					args = args.pcdr.(Pair)
				}
				e = args.pcar
				continue Loop
			case "define":
				sym := ep.pcdr.(Pair).pcar.AsS()
				exp := ep.pcdr.(Pair).pcdr.(Pair).pcar
				evalled, _ := p.evalEnv(env, exp)
				env.dict[sym] = evalled
				return NewAtom(false), nil
			case "set!":
				sym := ep.pcdr.(Pair).pcar.AsS()
				exp := ep.pcdr.(Pair).pcdr.(Pair).pcar
				evalled, _ := p.evalEnv(env, exp)
				env.replace(sym, evalled)
				return NewAtom(false), nil
			case "define-syntax":
				keyword := ep.pcdr.(Pair).pcar.AsS()
				transformer := ep.pcdr.(Pair).pcdr.(Pair).pcar.(Pair)
				macromap[keyword] = syntaxRules(keyword, transformer)
				return NewAtom(false), nil
			case "lambda":
				params := ep.pcdr.(Pair).pcar.(Pair)
				body := ep.pcdr.(Pair).pcdr.(Pair).pcar
				return Proc{se: se{
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
		args := []SE{}
		for pargs != NP(nil, nil) {
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

func (s se) IsS() bool {
	return s.isAtom && s.isString
}

func (s se) IsA() bool {
	return s.isExpression && s.isAtom
}

func (s se) IsP() bool {
	return s.isExpression && !s.isAtom
}

func (s se) AsS() string {
	return s.value.(string)
}

func (s se) AsF() float64 {
	return s.value.(float64)
}

func Newstring(s string) Atom {
	a := NewAtom(s)
	a.isString = true
	return a
}

func NewAtom(v interface{}) Atom {
	return Atom{se{
		isExpression: true,
		isAtom:       true,
		value:        v,
	}}
}

func NP(car, cdr SE) Pair {
	return Pair{
		se: se{
			isExpression: true,
		},
		pcar: car,
		pcdr: cdr,
	}
}

func list2cons(list ...SE) Pair {
	if len(list) == 0 {
		return NP(nil, nil)
	}
	if len(list) == 1 {
		return NP(list[0], NP(nil, nil))
	}
	cons := NP(nil, nil)
	for i := len(list) - 1; i >= 0; i-- {
		cons = NP(list[i], cons)
	}
	return cons
}

func cons2list(p Pair) []SE {
	list := []SE{}
	for p != NP(nil, nil) {
		list = append(list, p.pcar)
		p = p.pcdr.(Pair)
	}
	return list
}

func add(p *process, env *Env, args []SE) (SE, error) {
	return NewAtom(args[0].AsF() + args[1].AsF()), nil
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

func mustParse(program string) SE {
	p, _ := parse(program)
	return p
}

func parse(program string) (SE, error) {
	p, _, err := readFromTokens(tokenize(program))
	return p, err
}

func expandMacro(p Pair) (SE, bool) {
	if !p.pcar.IsS() {
		return p, false
	}
	s := p.pcar.AsS()
	tf, ok := macromap[s]
	if !ok {
		return p, false
	}
	return tf(p), true
}

func syntaxRules(keyword string, sr Pair) transformer {
	literals := []string{keyword, "lambda", "define", "begin"}
	for _, e := range cons2list(sr.pcdr.(Pair).pcar.(Pair)) {
		literals = append(literals, e.AsS())
	}
	clauses := prepareClauses(sr, literals)
	return generateTransformerFunction(clauses)
}

func prepareClauses(sr Pair, literals []string) []clause {
	clauses := []clause{}
	for _, c := range cons2list(sr.pcdr.(Pair).pcdr.(Pair)) {
		cp := c.(Pair)
		s := map[string]string{}
		e := map[string]int{}
		p := analysePattern(literals, cp.pcar, s, e)
		t := analyseTemplate(literals, cp.pcdr.(Pair).pcar, s, e)
		clauses = append(clauses, clause{pattern: p, template: t, ellipsis: e})
	}
	return clauses
}

func generateTransformerFunction(clauses []clause) transformer {
	return func(p Pair) SE {
		substitutions := map[string]SE{}
		fmt.Printf("in closure: clause[0].pattern.isList: %v\n", clauses[0].pattern.isList)
		unify(clauses[0].pattern, p, substitutions)
		return substituteTemplate(clauses[0].template, substitutions, clauses[0].ellipsis)
	}
}

var symbolCounter int

func gensym() string {
	symbolCounter += 1
	return string(fmt.Sprintf("gensym%d", symbolCounter))
}

func analyse(l []string, p SE, g map[string]string, b bool) pattern {
	if p.IsS() {
		s := p.AsS()
		if s == underscore {
			return pattern{isUnderscore: true}
		}
		for _, lt := range l {
			if lt == s {
				return pattern{isLiteral: true, content: p}
			}
		}
		if b {
			ns := gensym()
			g[s] = ns
			return pattern{isVariable: true, content: Newstring(ns)}
		}
		if ns, ok := g[s]; ok {
			return pattern{isVariable: true, content: Newstring(ns)}
		}
		return pattern{isVariable: true, content: p}
	}
	lc := []pattern{}
	list := cons2list(p.(Pair))
	for i := 0; i < len(list); i++ {
		pi := analyse(l, list[i], g, b)
		if i != len(list)-1 {
			sj := list[i+1]
			if sj.IsS() && sj.AsS() == ellipsis {
				pi.hasEllipsis = true
				i++
			}
		}
		lc = append(lc, pi)
	}
	return pattern{isList: true, listContent: lc}
}

func analysePattern(l []string, p SE, g map[string]string, e map[string]int) pattern {
	pt := analyse(l, p, g, true)
	analyseEllipsis(pt, e, 0)
	return pt
}

func analyseTemplate(l []string, t SE, g map[string]string, e map[string]int) pattern {
	return analyse(l, t, g, false)
}

func analyseEllipsis(p pattern, e map[string]int, d int) {
	if p.isVariable && (d != 0 || p.hasEllipsis) {
		ps := p.content.AsS()
		if p.hasEllipsis {
			d++
		}
		e[ps] = d
	} else if p.isList {
		nd := d
		if p.hasEllipsis {
			nd++
		}
		for _, pp := range p.listContent {
			analyseEllipsis(pp, e, nd)
		}
	}
}

func unify(p pattern, q SE, s map[string]SE) bool {
	return unifyWithEllipsis(p, q, s, []int{})
}

func unifyWithEllipsis(p pattern, q SE, s map[string]SE, d []int) bool {
	if p.isUnderscore || p.isVariable {
		if p.isVariable {
			ps := p.content.AsS()
			for i := range d {
				ps += fmt.Sprintf("#%d", d[i])
			}
			s[ps] = q
		}
		return true
	}
	if !p.isList {
		panic("pattern assumed to be list")
	}
	qp := q.(Pair)
	for _, pp := range p.listContent {
		if pp.hasEllipsis {
			nd := make([]int, len(d))
			copy(nd, d)
			nd = append(nd, 0)
			for qp != NP(nil, nil) {
				unifyWithEllipsis(pp, qp.pcar, s, nd)
				nd[len(nd)-1]++
				qp = qp.pcdr.(Pair)
			}
			continue
		}
		unifyWithEllipsis(pp, qp.pcar, s, d)
		qp = qp.pcdr.(Pair)
	}
	return qp == NP(nil, nil)
}

func substituteTemplate(p pattern, s map[string]SE, e map[string]int) SE {
	x, _ := substituteTemplateWithEllipsis(p, s, e, []int{})
	return x
}

func substituteTemplateWithEllipsis(template pattern, substitutions map[string]SE, e map[string]int, depth []int) (SE, bool) {
	if template.isLiteral {
		return template.content, false
	}
	if template.isVariable {
		ss := template.content.AsS()
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
		ss = template.content.AsS()
		s, ok = substitutions[ss]
		if ok {
			return s, false
		}
		newsym := Newstring(gensym())
		substitutions[ss] = newsym
		return newsym, false
	}
	out := []SE{}
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

func Multiparse(f string) ([]SE, error) {
	var e SE
	t := tokenize(f)
	exprs := []SE{}
	for len(t) > 0 {
		e, t, _ = readFromTokens(t)
		exprs = append(exprs, e)
	}
	return exprs, nil
}

func tokenize(s string) []string {
	return strings.Fields(strings.ReplaceAll(strings.ReplaceAll(s, "(", " ( "), ")", " ) "))
}

func readFromTokens(t []string) (SE, []string, error) {
	t0 := t[0]
	t = t[1:]
	switch t0 {
	case "(":
		list := []SE{}
		for t[0] != ")" {
			parsed, t1, _ := readFromTokens(t)
			t = t1
			list = append(list, parsed)
		}
		return list2cons(list...), t[1:], nil
	default:
		return atom(t0), t, nil
	}
}

func atom(t string) SE {
	if t == "0" || t == "1" {
		return NewAtom(float64(t[0] - '0'))
	}
	if t[0] == '\'' {
		q, _, _ := readFromTokens([]string{"(", "quote", t[1:], ")"})
		return q
	}
	return Newstring(t)
}
