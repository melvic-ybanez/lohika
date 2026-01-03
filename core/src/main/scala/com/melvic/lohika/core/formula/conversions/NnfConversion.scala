package com.melvic.lohika.core.formula.conversions

import cats.Endo
import com.melvic.lohika.core.formula.Formula
import Formula.*

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
        case Not(Forall(vars, scope)) =>
          convertExistential(recurse)(ThereExists(vars, !scope))
        case Not(ThereExists(vars, scope)) =>
          convertUniversal(recurse)(Forall(vars, !scope))
        case Forall(vars, Not(fm))      => Forall(vars, recurse(Not(fm)))
        case ThereExists(vars, Not(fm)) => ThereExists(vars, recurse(Not(fm)))
        case fm                         => convertBy(recurse)(fm)

      NegationsInside(recurse(fm))

  def simplifyNegations: NegationsInside => SimplifiedNegations =
    case NegationsInside(fm) => SimplifiedNegations(simplifyNegationsRaw(fm))

  private def simplifyNegationsRaw: Endo[Formula] =
    case Not(Not(p))                  => simplifyNegationsRaw(p)
    case Not(predicate: PredicateApp) => !predicate
    case not: Not                     => not
    case fm                           => convertBy(simplifyNegationsRaw)(fm)
