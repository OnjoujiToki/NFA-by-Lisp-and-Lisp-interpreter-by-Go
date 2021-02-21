package sexpr

import "errors"

// ErrParser is the error value returned by the Parser if the string is not a
// valid term.
// See also https://golang.org/pkg/errors/#New
// and // https://golang.org/pkg/builtin/#error
var ErrParser = errors.New("parser error")
var ErrGetRP = errors.New("get RP")
var ErrGetDot = errors.New("get Dot")
var ErrGetLP = errors.New("get LP")
var ErrGetQuote = errors.New("get Quote")
var ErrGetNIL = errors.New("get NIL")

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

func getSExprInPair(lexer *lexer) (*SExpr, error) {
	tok, err := lexer.next()
	if err != nil {
		return nil, ErrParser
	}
	if tok.typ == tokenNumber {
		return mkNumber(tok.num), nil
	}
	if tok.typ == tokenSymbol {
		if tok.literal == "NIL" {
			return mkNil(), nil
		}
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
		return nil, ErrGetLP
	}

	if tok.typ == tokenQuote {
		return nil, ErrGetQuote
	}

	return nil, err
}

func mkQuote(lexer *lexer) (*SExpr, error) {
	quoteHead := mkSymbol("QUOTE")
	val, err := getSExprInPair(lexer)

	if err == ErrGetLP {
		quoteList, suberr := getSExpr(lexer)
		if suberr != nil {
			return nil, ErrParser
		}
		quoteBody := mkConsCell(quoteList, mkNil())
		quoteExpr := mkConsCell(quoteHead, quoteBody)
		return quoteExpr, nil
	} else if err == ErrGetQuote {
		quoteList, suberr := mkQuote(lexer)
		if suberr != nil {
			return nil, ErrParser
		}
		quoteBody := mkConsCell(quoteList, mkNil())
		quoteExpr := mkConsCell(quoteHead, quoteBody)
		return quoteExpr, nil
	} else {
		quoteBody := mkConsCell(val, mkNil())
		quoteExpr := mkConsCell(quoteHead, quoteBody)
		return quoteExpr, nil
	}
}

func getSExpr(lexer *lexer) (*SExpr, error) {
	// read until r

	r := new(SExpr)
	temp := r

	err := ErrParser
	err = nil

	for true {

		temp.car, err = getSExprInPair(lexer)

		if err == ErrParser {
			return nil, ErrParser
		}

		if err == ErrGetLP {
			val, err := getSExpr(lexer)
			if err == nil {
				temp.car = val
			} else {
				return nil, ErrParser
			}
		}

		if err == ErrGetQuote {

			temp.car, err = mkQuote(lexer)

		}

		if err == ErrGetDot {
			next, suberr := getSExprInPair(lexer)
			if suberr == ErrGetLP {
				temp1, _ := getSExpr(lexer)
				temp.car = temp1.car
				temp.cdr = temp1.cdr
				temp.atom = temp1.atom
				_, RPerr := getSExprInPair(lexer)
				if RPerr == ErrGetRP {
					return r, nil
				} else {
					return nil, ErrParser
				}

			} else if suberr != ErrParser {
				temp.atom = next.atom
				temp.car = next.car
				temp.cdr = next.cdr
				_, RPerr := getSExprInPair(lexer)
				if RPerr == ErrGetRP {
					return r, nil
				} else {
					return nil, ErrParser
				}
			} else {
				return nil, ErrParser
			}
		}

		if err == ErrGetRP {

			return r, nil
		}

		temp.cdr = mkNil()

		temp = temp.cdr

	}
	return nil, ErrParser
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
		return mkQuote(lexer)
	}

	if tok.typ == tokenLpar {
		r, err := getSExpr(lexer)
		if err == nil {
			return r, nil
		} else {
			return nil, ErrParser
		}

		/*
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
		*/
	}
	return nil, ErrParser
}

func NewParser() Parser {
	var p myParserInstance
	return p
}
