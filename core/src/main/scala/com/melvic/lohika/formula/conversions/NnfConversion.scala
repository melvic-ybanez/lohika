package com.melvic.lohika.formula.conversions

import cats.Endo
import com.melvic.lohika.formula.Formula
import com.melvic.lohika.formula.Formula.*

/**
 * Negation Normal Form
 */
private[formula] trait NnfConversion:
  private[formula] final case class NegationsInside(raw: Formula)
  private[formula] final case class SimplifiedNegations(raw: Formula)

  def moveNegationsInside: NoIf => NegationsInside =
    case NoIf(fm) =>
      def recurse: Endo[Formula] =
        case Not(Or(p, q, rs))    => recurse(And(!p, !q, rs.map(!_)))
        case Not(And(p, q, rs))   => recurse(Or(!p, !q, rs.map(!_)))
        case notNot @ Not(Not(_)) => recurse(simplifyNegationsRaw(notNot))
        case Not(Forall(vars, matrix)) =>
          convertExistential(recurse)(ThereExists(vars, !matrix))
        case Not(ThereExists(vars, matrix)) =>
          convertUniversal(recurse)(Forall(vars, !matrix))
        case Forall(vars, Not(matrix))      => Forall(vars, recurse(Not(matrix)))
        case ThereExists(vars, Not(matrix)) => Forall(vars, recurse(Not(matrix)))
        case fm                             => convertBy(recurse)(fm)

      NegationsInside(recurse(fm))

  def simplifyNegations: NegationsInside => SimplifiedNegations =
    case NegationsInside(fm) => SimplifiedNegations(simplifyNegationsRaw(fm))

  private def simplifyNegationsRaw: Endo[Formula] =
    case Not(Not(p))     => simplifyNegationsRaw(p)
    case Not(True)       => False
    case Not(False)      => True
    case Not(p @ Var(_)) => !p
    case not: Not        => not
    case fm              => convertBy(simplifyNegationsRaw)(fm)
