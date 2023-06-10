package main

// http://norvig.com/lispy.html

import (
	"os"

	"github.com/deosjr/whistle/datalog"
	"github.com/deosjr/whistle/erlang"
	"github.com/deosjr/whistle/kanren"
	"github.com/deosjr/whistle/lisp"
)

func main() {
	filename := os.Args[1]
	sexpressions, _ := lisp.ParseFile(filename)
	l := lisp.New()
	kanren.Load(l)
	erlang.Load(l)
	datalog.Load(l)
	for _, e := range sexpressions {
		l.EvalExpr(e)
	}
}
