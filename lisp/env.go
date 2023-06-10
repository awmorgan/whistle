package lisp

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
			evarg, err := p.evalEnv(env, arg)
			if err != nil {
				return nil, err
			}
			args[i] = evarg
		}
		if proc.isBuiltin {
			return proc.builtin()(p, env, args)
		}
		defproc := proc.defined()
		env, e = newEnv(defproc.params, args, defproc.env), defproc.body
	}
}
