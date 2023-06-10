package lisp

import (
	"fmt"
	"math/rand"
	"sync"
)

type process struct {
	sync.Mutex
	pid string
}


func newProcess() *process {
	p := &process{
		pid: pidFunc(),
	}
	go func() {
	}()
	return p
}

var pidFunc func() string = generatePid

func generatePid() string {
	return "<pid" + fmt.Sprint(rand.Intn(9999999999)) + ">"
}
