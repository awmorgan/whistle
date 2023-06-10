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

func (p *process) Errorf(k continuation, v SExpression, s string, args ...any) SExpression {
	return NewPrimitive(processError{
		err: fmt.Errorf(s, args...),
		pid: p.pid,
		k:   k,
		v:   v,
	})
}

var mailboxes = make(map[string]chan SExpression)
var errchannels = make(map[string]chan processError)
var processlinks = make(map[string]map[string]struct{})

// TODO: move to erlang package, first need to figure out
// how to untangle the evals in receive builtin func
func LoadErlang(l Lisp) {
	p, env := l.process, l.Env

	// receive as 2 macros AND a function call...
	macromap["receive"] = syntaxRules("receive", mustParse(`(syntax-rules (receive_builtin receive_ after ->)
        ((_ ((v ...) rest ... ) ... (after millis -> expression ...))
         (receive_builtin (quote (after millis expression ...)) (receive_ (v ...) rest ...) ...))
        ((_ ((v ...) rest ... ) ...)
         (receive_builtin (receive_ (v ...) rest ...) ...)))`).AsPair())
	macromap["receive_"] = syntaxRules("receive_", mustParse(`(syntax-rules (when -> run fresh equalo)
        ((_ (vars ...) pattern -> expression ...)
         (quasiquote ((vars ...) (unquote (lambda (msg)
           (run 1 (fresh (q vars ...) (equalo q (quasiquote ((unquote vars) ...))) (equalo msg pattern) )))) expression ...)))
        ((_ (vars ...) pattern (when guard ...) -> expression ...)
         (quasiquote ((vars ...) (unquote (lambda (msg)
           (run 1 (fresh (q vars ...) (equalo q (quasiquote ((unquote vars) ...))) (equalo msg pattern) guard ...)))) expression ...))))`).AsPair())

	// these "builtins" can be defined in code now
	p.evalEnv(env, mustParse(`(define flush (lambda () (receive ((x) x ->
        (display (self)) (display " got ") (display x) (display newline) (flush))
        (after 0 -> #t))))`))
	p.evalEnv(env, mustParse("(define sleep (lambda (t) (receive (after t -> #t))))"))
}

var pidFunc func() string = generatePid

// TODO: until this lives in erlang package
func SetPidFuncForTest() {
	pidi := 0
	pidFunc = func() string {
		pidi++
		return fmt.Sprintf("<%02d>", pidi)
	}
}

func generatePid() string {
	return "<pid" + fmt.Sprint(rand.Intn(9999999999)) + ">"
}
