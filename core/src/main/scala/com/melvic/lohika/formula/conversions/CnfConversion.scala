package com.melvic.lohika.formula.conversions

import cats.Endo
import com.melvic.lohika.formula.Cnf.*
import com.melvic.lohika.formula.Formula.*
import com.melvic.lohika.formula.{Cnf, Formula}

private[formula] trait CnfConversion:
  //noinspection ConvertibleToMethodValue
  def toCnf(using SkolemSuffix): Formula => Cnf =
    toCnfRaw andThen:
      case or: Or if or.components.forall(isLiteral) =>
        COr(or.components.map(toCnf(_).asInstanceOf[CLiteral]))
      case and: And if and.components.forall(isClause) =>
        CAnd(and.components.map(toCnf(_).asInstanceOf[Clause]))
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

  private[formula] def toCnfRaw(using SkolemSuffix): Endo[Formula] =
    fm => toCnfAllRaw(List(fm)).head

  private[formula] def toCnfAllRaw(fms: List[Formula])(using SkolemSuffix): List[Formula] =
    skolemizeAll(
      standardizeAll(
        fms.map(
          eliminateBiconditionals andThen eliminateImplications andThen moveNegationsInside andThen
            simplifyNegations
        )
      ).map(toPnf)
    ).map(dropUniversalQuantifiers andThen distributeOrOverAnds andThen flattenOrsAndAnds)
