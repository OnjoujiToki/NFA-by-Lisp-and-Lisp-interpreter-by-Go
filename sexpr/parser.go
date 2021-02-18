package sexpr

import "errors"

// ErrParser is the error value returned by the Parser if the string is not a
// valid term.
// See also https://golang.org/pkg/errors/#New
// and // https://golang.org/pkg/builtin/#error
var ErrParser = errors.New("parser error")

//
// <sexpr>       ::= <atom> | <pars> | QUOTE <sexpr>
// <atom>        ::= NUMBER | SYMBOL
// <pars>        ::= LPAR <dotted_list> RPAR | LPAR <proper_list> RPAR
// <dotted_list> ::= <proper_list> <sexpr> DOT <sexpr>
// <proper_list> ::= <sexpr> <proper_list> | \epsilon
//
type Parser interface {
	Parse(string) (*SExpr, error)
}

type myParserInstance struct{}

func (myParserInstance) Parse(s string) (*SExpr, error) {
	lexer := newLexer(s)

	//read the first line
	tok, err := lexer.next()

	if err != nil {
		return nil, ErrParser
	}

	//EOF
	if tok.typ == tokenEOF {
		return mkNil(), nil
	}

	//if the first NUMBER
	if tok.typ == tokenNumber {
		subtok, suberr := lexer.next()
		if suberr != nil || subtok.typ != tokenEOF {
			return nil, ErrParser
		}
		return mkNumber(tok.num), nil
	}

	//if the first Symbol
	if tok.typ == tokenSymbol {
		subtok, suberr := lexer.next()
		if suberr != nil || subtok.typ != tokenEOF {
			return nil, ErrParser
		}
		return mkSymbol(tok.literal), nil
	}

}

func NewParser() Parser {
	panic("TODO: implement NewParser")
}
