package main

import (
	_ "embed"
	"os"

	"github.com/deosjr/whistle/lisp"
)

func main() {
	filename := os.Args[1]
	sexpressions, _ := lisp.ParseFile(filename)
	l := lisp.New()
	l.Load(datalog)
	for _, e := range sexpressions {
		l.EvalExpr(e)
	}
}

//go:embed datalog.lisp
var datalog string
