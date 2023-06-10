package lisp

import (
	"math"
)

func GlobalEnv() *Env {
	return &Env{dict: map[Symbol]SExpression{
		"+":            builtinFunc(add),
		"#t":           NewPrimitive(true),
		"#f":           NewPrimitive(false),
		"pi":           NewPrimitive(math.Pi),
		"newline":      NewPrimitive("\n"),
		"make-hashmap": builtinFunc(makeHashmap),
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

func makeHashmap(p *process, env *Env, args []SExpression) (SExpression, error) {
	return NewPrimitive(map[SExpression]SExpression{}), nil
}
