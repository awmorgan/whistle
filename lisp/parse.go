package lisp

import (
	"os"
	"strconv"
	"strings"
)

// TODO: read from stream of input, maybe eval as we parse valid sexpressions
// For now, we just slurp in the entire file and return a list of expressions
func ParseFile(filename string) ([]SExpression, error) {
	b, _ := os.ReadFile(filename)
	return Multiparse(string(b))
}

func Multiparse(file string) ([]SExpression, error) {
	tokens := tokenize(file)
	exprs := []SExpression{}
	for len(tokens) > 0 {
		e, rem, _ := readFromTokens(tokens)
		exprs = append(exprs, e)
		tokens = rem
	}
	return exprs, nil
}

func mustParse(program string) SExpression {
	p, _ := parse(program)
	return p
}

func parse(program string) (SExpression, error) {
	s := tokenize(program)
	p, _, err := readFromTokens(s)
	return p, err
}

func tokenize(s string) []string {
	s = strings.ReplaceAll(s, "[", "(")
	s = strings.ReplaceAll(s, "]", ")")
	s = strings.ReplaceAll(s, "(", " ( ")
	s = strings.ReplaceAll(s, ")", " ) ")
	tokenized := []string{}
	fields := strings.Fields(s)
	// pasting string escaped stuff back together..
	str := ""
	comment := false
	for i := 0; i < len(fields); i++ {
		ss := fields[i]
		if len(str) == 0 && ss == "#|" {
			comment = true
			continue
		}
		if len(str) == 0 && comment && ss == "|#" {
			comment = false
			continue
		}
		if comment {
			continue
		}
		if len(str) == 0 && strings.HasPrefix(ss, `"`) {
			if len(ss) > 1 && strings.HasSuffix(ss, `"`) {
				tokenized = append(tokenized, ss)
				continue
			}
		}
		tokenized = append(tokenized, ss)
	}
	return tokenized
}

func readFromTokens(tokens []string) (SExpression, []string, error) {
	token := tokens[0]
	tokens = tokens[1:]
	switch token {
	case "(":
		list := []SExpression{}
		for tokens[0] != ")" {
			parsed, t, _ := readFromTokens(tokens)
			tokens = t
			list = append(list, parsed)
		}
		return list2cons(list...), tokens[1:], nil
	default:
		return atom(token), tokens, nil
	}
}

func atom(token string) SExpression {
	if n, err := strconv.ParseFloat(token, 64); err == nil {
		return NewPrimitive(n)
	}
	if token[0] == token[len(token)-1] && token[0] == '"' {
		return NewPrimitive(token[1 : len(token)-1])
	}
	// TODO unquote syntax only works on symbols, not lists atm!
	if token[0] == ',' {
		unquote, _, _ := readFromTokens([]string{"(", "unquote", token[1:], ")"})
		return unquote
	}
	// TODO quote syntax only works on symbols, not lists atm!
	if token[0] == '\'' {
		quote, _, _ := readFromTokens([]string{"(", "quote", token[1:], ")"})
		return quote
	}
	return NewSymbol(token)
}
