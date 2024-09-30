package com.melvic.lohika.formula

import cats.Endo
import com.melvic.lohika.formula.Converter.{
  convertConjunction,
  convertUniversal,
  convertNegation,
  convertDisjunction,
  convertExistential
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
      convertExistential(moveNegationsInside)(ThereExists(vars, !matrix))
    case Not(ThereExists(vars, matrix)) =>
      convertUniversal(moveNegationsInside)(Forall(vars, !matrix))
    case not: Not => convertNegation(moveNegationsInside)(not)
    case or: Or   => convertDisjunction(moveNegationsInside)(or)
    case and: And => convertConjunction(moveNegationsInside)(and)
    case fm       => fm

  def simplifyNegations: Endo[Formula] =
    case Not(Not(p))     => simplifyNegations(p)
    case Not(True)       => False
    case Not(False)      => True
    case Not(p @ Var(_)) => !p
    case not: Not        => not
    case or: Or          => convertDisjunction(simplifyNegations)(or)
    case and: And        => convertConjunction(simplifyNegations)(and)
    case fm              => fm