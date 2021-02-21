package sexpr

import "errors"

// ErrParser is the error value returned by the Parser if the string is not a
// valid term.
// See also https://golang.org/pkg/errors/#New
// and // https://golang.org/pkg/builtin/#error
var ErrParser = errors.New("parser error")
var ErrGetRP = errors.New("get RP")
var ErrGetDot = errors.New("get Dot")

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

func getSExprAfterQuote(lexer *lexer) (*SExpr, error) {
	//read the first line
	tok, err := lexer.next()

	if err != nil {
		return nil, ErrParser
	}

	//EOF
	if tok.typ == tokenEOF {
		return nil, ErrParser
	}

	if tok.typ == tokenEOF {
		return nil, ErrParser
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

	if tok.typ == tokenLpar {
		r := new(SExpr)
		temp := r
		for true {
			temp.car, err = getSExprInPair(lexer)
			if err == ErrParser {
				return nil, ErrParser
			}
			temp.cdr = mkNil()
			temp = temp.cdr
			if err == ErrGetRP {
				if tok, err := lexer.next(); err != nil || tok.typ != tokenEOF {
					return nil, ErrParser
				}
				return r, nil
			}

		}
	}
	return nil, ErrParser
}

func getSExprInPair(lexer *lexer) (*SExpr, error) {
	tok, err := lexer.next()
	if err != nil {
		return nil, ErrParser
	}
	if tok.typ == tokenNumber {
		return mkNumber(tok.num), nil
	}
	if tok.typ == tokenSymbol {
		return mkSymbol(tok.literal), nil
	}

	if tok.typ == tokenRpar {
		return nil, ErrGetRP
	}

	if tok.typ == tokenDot {
		return nil, ErrGetDot
	}

	if tok.typ == tokenEOF {
		return nil, ErrParser
	}

	if tok.typ == tokenLpar {
		r := new(SExpr)
		temp := r

		for true {
			tok, err := lexer.next()
			if err != nil {
				return nil, ErrParser
			}
			if tok.typ == tokenNumber {
				temp.car = mkNumber(tok.num)
				temp.cdr = mkNil()
				temp = temp.cdr
				continue
			}
			if tok.typ == tokenSymbol {
				temp.car = mkSymbol(tok.literal)
				temp.cdr = mkNil()
				temp = temp.cdr
				continue
			}
			if tok.typ == tokenDot {
				temp1, _ := getSExprInPair(lexer)
				temp.car = temp1.car
				temp.atom = temp1.atom
				temp.cdr = temp1.cdr
				continue
			}
			if tok.typ == tokenLpar {
				temp.car, err = getSExprInPair(lexer)
				temp.cdr = mkNil()
				temp = temp.cdr
				continue
			}

			if tok.typ == tokenEOF {
				return r, ErrParser
			}

			if tok.typ == tokenRpar {
				//r.cdr = mkNil()
				return r, nil
			}

		}
	}

	return nil, err
}

func (myParserInstance) Parse(s string) (*SExpr, error) {
	lexer := newLexer(s)

	//read the first line
	tok, err := lexer.next()

	if err != nil {
		return nil, ErrParser
	}

	//EOF
	if tok.typ == tokenEOF {
		return nil, ErrParser
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

	if tok.typ == tokenQuote {
		quote := new(SExpr)
		quote.car = mkSymbol("QUOTE")
		quote.cdr, err = getSExprAfterQuote(lexer)
		if err == nil {
			return quote, nil
		} else {
			return nil, ErrParser
		}
	}

	if tok.typ == tokenLpar {
		r := new(SExpr)
		temp := r
		for true {
			temp.car, err = getSExprInPair(lexer)
			if err == ErrParser {
				return nil, ErrParser
			}

			if err == ErrGetDot {
				temp1, err := getSExprInPair(lexer)
				temp.atom = temp1.atom
				temp.car = temp1.car
				temp.cdr = temp1.cdr
				if err == nil {
					return r, nil
				}
			}

			if err == ErrGetRP {
				if tok, err := lexer.next(); err != nil || tok.typ != tokenEOF {
					return nil, ErrParser
				}
				return r, nil
			}
			temp.cdr = mkNil()
			temp = temp.cdr

		}
	}

	return nil, ErrParser

}

func NewParser() Parser {
	var p myParserInstance
	return p
}
