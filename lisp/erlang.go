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

type processError struct {
}

func newProcess() *process {
	p := &process{
		pid: pidFunc(),
	}
	ch := make(chan SExpression)
	mailboxes[p.pid] = ch
	errch := make(chan processError)
	errchannels[p.pid] = errch
	processlinks[p.pid] = map[string]struct{}{}
	go func() {
	}()
	return p
}

var mailboxes = make(map[string]chan SExpression)
var errchannels = make(map[string]chan processError)
var processlinks = make(map[string]map[string]struct{})

var pidFunc func() string = generatePid

func generatePid() string {
	return "<pid" + fmt.Sprint(rand.Intn(9999999999)) + ">"
}
