package main

import (
	"os"

	"github.com/deosjr/whistle/datalog"
	"github.com/deosjr/whistle/lisp"
)

func main() {
	filename := os.Args[1]
	sexpressions, _ := lisp.ParseFile(filename)
	l := lisp.New()
	datalog.Load(l)
	for _, e := range sexpressions {
		l.EvalExpr(e)
	}
}
