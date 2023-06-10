package lisp

import (
	"fmt"
	"math/rand"
	"sync"
)

// https://learnyousomeerlang.com/the-hitchhikers-guide-to-concurrency
// adding concurrency to our LISP based on actor model of Erlang,
// but implemented using Golang's CSP (which is different!)

// For this second attempt we will spawn each process with a copy of the env
// in which the process was spawned, and storing '$PID' on the process struct

// TODO: this file is only partially migrated to erlang/erlang.go due to
// some very nasty dependencies on eval within receive!

type process struct {
	sync.Mutex
	pid     string
	mailbox []SExpression
	err     error
	// process flags
	trapExit             bool
	evalWithContinuation bool
	// TODO: rand seed, etc
}

type processError struct {
	err error
	pid string
	// if we encounter error and we are in evalK, we can restore
	// using value under evaluation (v) and continuation (k)
	k continuation
	v SExpression
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
		for {
			select {
			case msg := <-ch:
				p.Lock()
				p.mailbox = append(p.mailbox, msg)
				p.Unlock()
			case perr := <-errch:
				if p.trapExit && perr.pid != p.pid {
					go func() {
						ch <- list2cons(NewSymbol("EXIT"), NewSymbol(perr.pid), NewPrimitive(perr.err.Error()))
					}()
					continue
				}
				p.err = perr.err
				for link := range processlinks[p.pid] {
					errchannels[link] <- perr
				}
				return
			}
		}
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
