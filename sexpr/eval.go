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
	}

	if funcName(expr) == "QUOTE" {
		return expr.cdr.car, nil
	}

	if funcName(expr) == "CAR" {

		if expr.cdr.car.isNil() {
			return mkNil(), nil
		}
		return expr.cdr.car.cdr.car.car, nil
	}

	if funcName(expr) == "CDR" {

		if expr.cdr.car.isNil() {
			return mkNil(), nil
		}
		return expr.cdr.car.cdr.car.cdr, nil
	}

	if funcName(expr) == "CONS" {
		first, _ := expr.cdr.car.Eval()
		second, _ := expr.cdr.cdr.car.Eval()

		return mkConsCell(first, second), nil
	}

	if funcName(expr) == "ATOM" {
		first, _ := expr.cdr.car.Eval()
		if first.car != nil || first.cdr != nil {
			return mkNil(), nil
		} else {
			return mkSymbolTrue(), nil
		}
	}
	if funcName(expr) == "LISTP" {
		first, _ := expr.cdr.car.Eval()
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
			add, _ := temp.car.Eval()
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
			add, _ := temp.car.Eval()
			r.Mul(r, add.atom.num)
			temp = temp.cdr
			if temp.isNil() {
				break
			}
		}
		return mkNumber(r), nil
	}

	if funcName(expr) == "ZEROP" {
		first, _ := expr.cdr.car.Eval()
		y := big.NewInt(0)
		if first.atom.num.Cmp(y) == 0 {
			return mkSymbolTrue(), nil
		} else {
			return mkNil(), nil
		}
	}

	if funcName(expr) == "LENGTH" {
		r := big.NewInt(0)
		temp := expr.cdr.car.cdr.car
		if temp.isNil() {
			return mkNumber(r), nil
		}
		for true {
			r.Add(r, big.NewInt(1))
			temp = temp.cdr
			if temp.isNil() {
				break
			}
		}
		return mkNumber(r), nil

	}

	return mkNil(), nil
}
