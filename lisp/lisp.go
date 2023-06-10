package lisp

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
