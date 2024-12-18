package com.melvic.lohika.core.formula.conversions

import cats.Endo
import com.melvic.lohika.core.formula.{Cnf, Formula}
import com.melvic.lohika.core.formula.Cnf.*
import com.melvic.lohika.core.formula.Formula.*

private[formula] trait CnfConversion:
  def toCnf: Formula => Cnf =
    fm => toCnfAll(List(fm)).head

  def toCnfAll: List[Formula] => List[Cnf] =
    toCnfAllRaw(_).map:
      case or: Or if or.components.forall(isLiteral) =>
        COr(toCnfAll(or.components).map(_.asInstanceOf[CLiteral]))
      case and: And if and.components.forall(isClause) =>
        CAnd(toCnfAll(and.components).map(_.asInstanceOf[CLiteral]))
      case Not(predicateApp: PredicateApp) => CNot(predicateApp)
      case predicateApp: PredicateApp      => predicateApp
      case fm                              => CAnd(Nil)

  def fromCnf: Cnf => Formula =
    case CAnd(p :: q :: rs)      => And(fromCnf(p), fromCnf(q), rs.map(fromCnf))
    case CAnd(p :: Nil)          => fromCnf(p)
    case CAnd(Nil)               => PredicateApp.True
    case COr(p :: q :: rs)       => Or(fromCnf(p), fromCnf(q), rs.map(fromCnf))
    case COr(p :: Nil)           => fromCnf(p)
    case COr(Nil)                => PredicateApp.False
    case CNot(p)                 => !fromCnf(p)
    case predicate: PredicateApp => predicate

  private[formula] def toCnfRaw: Endo[Formula] =
    fm => toCnfAllRaw(List(fm)).head

  private[formula] def toCnfAllRaw(fms: List[Formula]): List[Formula] =
    skolemizeAll(
      standardizeAll(
        fms.map(
          eliminateBiconditionals andThen eliminateImplications andThen moveNegationsInside andThen
            simplifyNegations
        )
      ).map(toPnf)
    ).map(dropUniversalQuantifiers andThen distributeOrOverAnds andThen flattenOrsAndAnds)
