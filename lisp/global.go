package lisp

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"reflect"
)

func GlobalEnv() *Env {
	return &Env{dict: map[Symbol]SExpression{
		"+":              builtinFunc(add),
		"-":              builtinFunc(sub),
		"*":              builtinFunc(mul),
		"/":              builtinFunc(div),
		"=":              builtinFunc(eq),
		"<":              builtinFunc(lt),
		">":              builtinFunc(gt),
		"<=":             builtinFunc(leq),
		">=":             builtinFunc(geq),
		"mod":            builtinFunc(mod),
		"#t":             NewPrimitive(true),
		"#f":             NewPrimitive(false),
		"pi":             NewPrimitive(math.Pi),
		"newline":        NewPrimitive("\n"),
		"number?":        builtinFunc(isnumber),
		"pair?":          builtinFunc(ispair),
		"car":            builtinFunc(car),
		"cdr":            builtinFunc(cdr),
		"cons":           builtinFunc(cons),
		"null?":          builtinFunc(isnull),
		"procedure?":     builtinFunc(isprocedure),
		"eqv?":           builtinFunc(isequivalent),
		"display":        builtinFunc(display),
		"exit":           builtinFunc(exit),
		"string-append":  builtinFunc(stringappend),
		"number->string": builtinFunc(number2string),
		"string->symbol": builtinFunc(string2symbol),
		"gensym":         builtinFunc(gensymFunc),
		"eval":           builtinFunc(eval),
		"read":           builtinFunc(read),
		"read-string":    builtinFunc(readString),
		"environment":    builtinFunc(environment),
		"listing":        builtinFunc(listing),
	}, outer: nil}
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

func sub(p *process, env *Env, args []SExpression) (SExpression, error) {
	return NewPrimitive(args[0].AsNumber() - args[1].AsNumber()), nil
}

func mul(p *process, env *Env, args []SExpression) (SExpression, error) {
	return NewPrimitive(args[0].AsNumber() * args[1].AsNumber()), nil
}

func div(p *process, env *Env, args []SExpression) (SExpression, error) {
	return NewPrimitive(args[0].AsNumber() / args[1].AsNumber()), nil
}

func eq(p *process, env *Env, args []SExpression) (SExpression, error) {
	return NewPrimitive(args[0].AsNumber() == args[1].AsNumber()), nil
}

func lt(p *process, env *Env, args []SExpression) (SExpression, error) {
	return NewPrimitive(args[0].AsNumber() < args[1].AsNumber()), nil
}

func gt(p *process, env *Env, args []SExpression) (SExpression, error) {
	return NewPrimitive(args[0].AsNumber() > args[1].AsNumber()), nil
}

func leq(p *process, env *Env, args []SExpression) (SExpression, error) {
	return NewPrimitive(args[0].AsNumber() <= args[1].AsNumber()), nil
}

func geq(p *process, env *Env, args []SExpression) (SExpression, error) {
	return NewPrimitive(args[0].AsNumber() >= args[1].AsNumber()), nil
}

func mod(p *process, env *Env, args []SExpression) (SExpression, error) {
	return NewPrimitive(math.Mod(args[0].AsNumber(), args[1].AsNumber())), nil
}

func isnumber(p *process, env *Env, args []SExpression) (SExpression, error) {
	return NewPrimitive(args[0].IsNumber()), nil
}

func ispair(p *process, env *Env, args []SExpression) (SExpression, error) {
	x := args[0]
	if !x.IsPair() {
		return NewPrimitive(false), nil
	}
	return NewPrimitive(x.AsPair() != empty), nil
}

func car(p *process, env *Env, args []SExpression) (SExpression, error) {
	return args[0].AsPair().car(), nil
}

func cdr(p *process, env *Env, args []SExpression) (SExpression, error) {
	return args[0].AsPair().cdr(), nil
}

func cons(p *process, env *Env, args []SExpression) (SExpression, error) {
	return NewPair(args[0], args[1]), nil
}

func isnull(p *process, env *Env, args []SExpression) (SExpression, error) {
	x := args[0]
	if x.IsProcedure() {
		return NewPrimitive(false), nil
	}
	if x.IsAtom() {
		return NewPrimitive(false), nil
	}
	return NewPrimitive(x.AsPair() == empty), nil
}

func isprocedure(p *process, env *Env, args []SExpression) (SExpression, error) {
	return NewPrimitive(args[0].IsProcedure()), nil
}

func isequivalent(p *process, env *Env, args []SExpression) (SExpression, error) {
	return NewPrimitive(reflect.DeepEqual(args[0], args[1])), nil
}

func display(p *process, env *Env, args []SExpression) (SExpression, error) {
	if args[0] == nil {
		return NewPrimitive(true), nil
	}
	if args[0].IsPrimitive() {
		if s, ok := args[0].AsPrimitive().(string); ok {
			fmt.Print(s)
			return NewPrimitive(true), nil
		}
	}
	fmt.Print(args[0])
	return NewPrimitive(true), nil
}

func exit(p *process, env *Env, args []SExpression) (SExpression, error) {
	ex := fmt.Errorf("normal")
	target := p.pid
	if len(args) == 1 {
		ex = fmt.Errorf("%s", args[0])
	}
	if len(args) > 1 {
		target = args[0].AsPrimitive().(string)
		ex = fmt.Errorf("%s", args[1])
	}
	errchannels[target] <- processError{err: ex, pid: p.pid}
	return nil, ex
}

func stringappend(p *process, env *Env, args []SExpression) (SExpression, error) {
	s := ""
	for _, arg := range args {
		s += arg.AsPrimitive().(string)
	}
	return NewPrimitive(s), nil
}

func number2string(p *process, env *Env, args []SExpression) (SExpression, error) {
	return NewPrimitive(fmt.Sprintf("%v", args[0].AsNumber())), nil
}

func string2symbol(p *process, env *Env, args []SExpression) (SExpression, error) {
	return NewSymbol(args[0].AsPrimitive().(string)), nil
}

func gensymFunc(p *process, env *Env, args []SExpression) (SExpression, error) {
	return NewSymbol(gensym()), nil
}

// (eval expression [env]), defaults to env=env
func eval(p *process, env *Env, args []SExpression) (SExpression, error) {
	if len(args) > 1 {
		env = args[1].AsPrimitive().(*Env)
	}
	var e SExpression
	var perr processError
	if p.evalWithContinuation {
		id := func(x SExpression) SExpression { return x }
		e = p.evalEnvK(env, args[0], id)
		/*
		   // TODO: enable to make evalK work with erlang err msg passing
		   if e != nil && e.IsPrimitive() {
		       if err, ok := e.AsPrimitive().(processError); ok {
		           perr = err
		       }
		   }
		*/
	} else {
		ev, err := p.evalEnv(env, args[0])
		e = ev
		perr = processError{err: err, pid: p.pid}
	}
	if perr.err != nil {
		errchannels[p.pid] <- perr
		return nil, perr.err
	}
	return e, nil
}

func read(p *process, env *Env, args []SExpression) (SExpression, error) {
	scanner := bufio.NewScanner(os.Stdin)
	scanner.Scan()
	return parse(scanner.Text())
}

func readString(p *process, env *Env, args []SExpression) (SExpression, error) {
	return parse(args[0].AsPrimitive().(string))
}

func environment(p *process, env *Env, args []SExpression) (SExpression, error) {
	return NewPrimitive(env), nil
}

func listing(p *process, env *Env, args []SExpression) (SExpression, error) {
	s := args[0].AsSymbol()
	e, ok := env.find(s)
	if !ok {
		return NewPrimitive(false), nil
	}
	sexp := e.dict[s]
	if !sexp.IsProcedure() {
		return NewPrimitive(false), nil
	}
	proc := sexp.AsProcedure()
	if proc.isBuiltin {
		fmt.Printf("builtin func %s\n", s)
		return NewPrimitive(true), nil
	}
	f := proc.defined()
	fmt.Printf("(define %s (lambda %s %s)", s, f.params, f.body)
	return NewPrimitive(true), nil
}