package com.melvic.lohika.formula

import cats.Endo
import com.melvic.lohika.formula.Converter.{
  fromAndWith,
  fromForallWith,
  fromNotWith,
  fromOrWith,
  fromThereExistsWith
}
import com.melvic.lohika.formula.Formula.*

/**
 * Negation Normal Form
 */
object Nnf:
  def moveNegationsInside: Endo[Formula] =
    case Not(Or(p, q, rs))    => moveNegationsInside(And(!p, !q, rs.map(!_)))
    case Not(And(p, q, rs))   => moveNegationsInside(Or(!p, !q, rs.map(!_)))
    case notNot @ Not(Not(_)) => moveNegationsInside(simplifyNegations(notNot))
    case Not(Forall(vars, matrix)) =>
      fromThereExistsWith(moveNegationsInside)(ThereExists(vars, !matrix))
    case Not(ThereExists(vars, matrix)) =>
      fromForallWith(moveNegationsInside)(Forall(vars, !matrix))
    case not: Not => fromNotWith(moveNegationsInside)(not)
    case or: Or   => fromOrWith(moveNegationsInside)(or)
    case and: And => fromAndWith(moveNegationsInside)(and)
    case fm       => fm

  def simplifyNegations: Endo[Formula] =
    case Not(Not(p))     => simplifyNegations(p)
    case Not(True)       => False
    case Not(False)      => True
    case Not(p @ Var(_)) => !p
    case not: Not        => not
    case or: Or          => fromOrWith(simplifyNegations)(or)
    case and: And        => fromAndWith(simplifyNegations)(and)
    case fm              => fm
