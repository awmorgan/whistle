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
