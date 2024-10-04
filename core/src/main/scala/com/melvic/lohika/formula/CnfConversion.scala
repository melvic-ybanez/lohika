package com.melvic.lohika.formula

import cats.Endo
import com.melvic.lohika.formula.Cnf.*
import com.melvic.lohika.formula.Formula.*

private[formula] trait CnfConversions:
  def toCnf: Formula => Cnf =
    toCnfUntyped andThen:
      case or: Or if or.components.forall(isLiteral) =>
        COr(or.components.map(toCnf(_).asInstanceOf[Literal]))
      case and: And if and.components.forall(isClause) =>
        CAnd(and.components.map(toCnf(_).asInstanceOf[Clause]))
      case Not(Var(name)) => CNot(CVar(name))
      case Var(name)      => CVar(name)
      case True           => CTrue
      case False          => CFalse
      case fm             => CAnd(Nil)

  def fromCnf: Cnf => Formula =
    case CAnd(p :: q :: rs) => And(fromCnf(p), fromCnf(q), rs.map(fromCnf))
    case CAnd(p :: Nil)     => fromCnf(p)
    case CAnd(Nil)          => True
    case COr(p :: q :: rs)  => Or(fromCnf(p), fromCnf(q), rs.map(fromCnf))
    case COr(p :: Nil)      => fromCnf(p)
    case COr(Nil)           => False
    case CNot(p)            => !fromCnf(p)
    case CVar(name)         => Var(name)
    case CTrue              => True
    case CFalse             => False

  def toCnfUntyped: Endo[Formula] =
    eliminateBiconditionals andThen
      eliminateImplications andThen
      moveNegationsInside andThen
      distributeOrOverAnds andThen
      simplifyNegations andThen
      flattenOrsAndAnds
