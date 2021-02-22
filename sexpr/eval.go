package sexpr

import (
	"errors"
	"math/big"
	//"math/big" // You will need to use this package in your implementation.
)

// ErrEval is the error value returned by the Evaluator if the contains
// an invalid token.
// See also https://golang.org/pkg/errors/#New
// and // https://golang.org/pkg/builtin/#error
var ErrEval = errors.New("eval error")

func deepth(expr *SExpr) int {
	deepth := 1
	temp := expr
	for true {
		deepth++
		if (temp.cdr).isNil() {
			break
		}
		temp = temp.cdr
	}
	return deepth
}
func single(expr *SExpr) bool {
	if expr.atom != nil {
		return true
	} else {
		return false
	}
}

func (expr *SExpr) CDR() *SExpr {
	return expr.cdr
}

func CAR(expr *SExpr) *SExpr {
	return expr.car
}

func singleName(expr *SExpr) string {
	val := expr.atom.String()
	return val
}

func funcName(expr *SExpr) string {
	return expr.car.atom.String()
}

func (expr *SExpr) Eval() (*SExpr, error) {

	if expr.isNil() {
		return mkNil(), nil
	}
	if single(expr) {
		if single(expr) && singleName(expr) == "*" {
			return nil, ErrEval
		}
		if single(expr) && singleName(expr) == "+" {
			return nil, ErrEval
		}
		if expr.isNumber() {
			return expr, nil
		}
		return nil, ErrEval
	}

	if funcName(expr) == "QUOTE" {
		if expr.cdr.cdr == nil {
			return nil, ErrEval
		}
		if !expr.cdr.cdr.isNil() {
			return nil, ErrEval
		}

		if expr.cdr.isNil() {
			return nil, ErrEval
		}

		return expr.cdr.car, nil
	}

	if funcName(expr) == "CAR" {
		if expr.cdr.isNil() {
			return nil, ErrEval
		}
		if expr.cdr.car.isNil() {
			return mkNil(), nil
		}
		if expr.cdr.car.isAtom() {
			return nil, ErrEval
		}
		if expr.cdr.car.cdr.car.isNil() {
			return nil, ErrEval
		}
		if !expr.cdr.cdr.isNil() {
			return nil, ErrEval
		}

		return expr.cdr.car.cdr.car.car, nil
	}

	if funcName(expr) == "CDR" {
		if expr.cdr.isNil() {
			return nil, ErrEval
		}

		if expr.cdr.car.isNil() {
			return mkNil(), nil
		}
		if expr.cdr.car.isAtom() {
			return nil, ErrEval
		}
		if expr.cdr.car.cdr.car.isNil() {
			return nil, ErrEval
		}
		return expr.cdr.car.cdr.car.cdr, nil
	}

	if funcName(expr) == "CONS" {
		if expr.cdr.isNil() {
			return nil, ErrEval
		}

		if expr.cdr.car == nil || expr.cdr.cdr.car == nil {
			return nil, ErrEval
		}

		first, _ := expr.cdr.car.Eval()
		second, _ := expr.cdr.cdr.car.Eval()

		if expr.cdr.cdr.cdr.car != nil {
			return nil, ErrEval
		}

		if first == nil || second == nil {
			return nil, ErrEval
		}

		return mkConsCell(first, second), nil
	}

	if funcName(expr) == "ATOM" {
		if expr.cdr.isNil() {
			return nil, ErrEval
		}
		if !expr.cdr.cdr.isNil() {
			return nil, ErrEval
		}
		first, _ := expr.cdr.car.Eval()
		if first == nil {
			return nil, ErrEval
		}
		if first.car != nil || first.cdr != nil {
			return mkNil(), nil
		} else {
			return mkSymbolTrue(), nil
		}
	}
	if funcName(expr) == "LISTP" {
		if expr.cdr.isNil() {
			return nil, ErrEval
		}
		if !expr.cdr.cdr.isNil() {
			return nil, ErrEval
		}
		first, _ := expr.cdr.car.Eval()
		if first == nil {
			return nil, ErrEval
		}
		if first.atom == nil {
			return mkSymbolTrue(), nil
		} else {
			return mkNil(), nil
		}
	}

	if funcName(expr) == "+" {
		r := big.NewInt(0)
		temp := expr.cdr
		if temp.isNil() {
			return mkNumber(r), nil
		}
		for true {
			number, _ := temp.car.Eval()
			if number == nil {
				return nil, ErrEval
			}
			if !number.isNumber() {
				return nil, ErrEval
			}
			add := number
			r.Add(r, add.atom.num)
			temp = temp.cdr
			if temp.isNil() {
				break
			}
		}
		return mkNumber(r), nil

	}

	if funcName(expr) == "*" {
		r := big.NewInt(1)
		temp := expr.cdr
		if temp.isNil() {
			return mkNumber(r), nil
		}
		for true {
			number, _ := temp.car.Eval()
			if number == nil {
				return nil, ErrEval
			}
			if !number.isNumber() {
				return nil, ErrEval
			}
			add := number

			r.Mul(r, add.atom.num)
			temp = temp.cdr
			if temp.isNil() {
				break
			}
		}
		return mkNumber(r), nil
	}

	if funcName(expr) == "ZEROP" {

		if expr.cdr.isNil() {
			return nil, ErrEval
		}
		if !expr.cdr.cdr.isNil() {
			return nil, ErrEval
		}
		first, _ := expr.cdr.car.Eval()
		if first == nil {
			return nil, ErrEval
		}
		if first.isNil() {
			return nil, ErrEval
		}
		y := big.NewInt(0)
		if first.atom.num.Cmp(y) == 0 {
			return mkSymbolTrue(), nil
		} else {
			return mkNil(), nil
		}
	}

	if funcName(expr) == "LENGTH" {

		if expr.cdr.isNil() {
			return nil, ErrEval
		}

		r := big.NewInt(0)
		if expr.cdr.car == nil || expr.cdr.car.cdr == nil {
			return nil, ErrEval
		}
		temp := expr.cdr.car.cdr.car
		if temp == nil {
			return nil, ErrEval
		}
		if temp.isNil() {
			return mkNumber(r), nil
		}
		for true {
			r.Add(r, big.NewInt(1))
			temp = temp.cdr
			if temp == nil {
				return nil, ErrEval
			}
			if temp.isNil() {
				break
			}
		}
		return mkNumber(r), nil

	}

	return nil, ErrEval
}
