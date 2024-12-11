package com.melvic.lohika.core.formula.conversions

import cats.Endo
import com.melvic.lohika.core.formula.Formula
import com.melvic.lohika.core.formula.Formula.*

/**
 * Moves the quantifiers outside.
 */
private[formula] trait PnfConversion:
  private[formula] final case class Pnf(raw: Formula)

  def toPnf: Standardized => Pnf =
    case Standardized(fm) =>
      def recurse: Endo[Formula] =
        case and: And => fromFList(and, And.of(_, _), And.fromList)
        case or: Or   => fromFList(or, Or.of(_, _), Or.fromList)
        case fm       => convertBy(recurse)(fm)

      def fromFList(
          fList: FList,
          connect: (Formula, Formula) => Formula,
          fromList: List[Formula] => Formula
      ): Formula =
        val comps = (fList.q :: fList.rs).foldLeft(List(fList.p)):
          case (Quantified(quantifier, boundVars, matrix) :: rest, fm) =>
            Quantified(quantifier, boundVars, connect(matrix, fm)) :: rest
          case (fm :: rest, Quantified(quantifier, boundVars, matrix)) =>
            Quantified(quantifier, boundVars, connect(fm, matrix)) :: rest
          case (acc, fm) => fm :: acc
        fromList(comps.reverse)

      Pnf(recurse(fm))
