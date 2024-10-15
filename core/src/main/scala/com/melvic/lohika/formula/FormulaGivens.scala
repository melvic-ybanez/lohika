package com.melvic.lohika.formula

import cats.{Endo, Eq, Show}
import Formula.*
import com.melvic.lohika.Formatter
import fastparse.*
import Formatter.*
import com.melvic.lohika.parsers.Parser

private[formula] trait FormulaGivens:
  given Conversion[String, Formula] = input =>
    Parser.parseFormula(input) match
      case Parsed.Success(fm: Formula, _) => fm
      case _                              => throw new Error(s"Unable to parse $input")

  given [E <: Expression](using Formatter): Show[E] =
    Show.show(Formula.prettyPrint(_).formula)

  given [F <: Formula]: Eq[F] = Eq.instance: (fm1, fm2) =>
    def compare(fm1: Formula, fm2: Formula): Boolean =
      def hasSameComps(
          selfFList: Formula,
          otherFList: Formula,
          flatten: Endo[Formula]
      ): Boolean =
        def flattenComponents(fList: Formula): List[Formula] = (flatten(fList): @unchecked) match
          case flatFList: FList => flatFList.components

        val selfComps = flattenComponents(selfFList)
        val otherComps = flattenComponents(otherFList)

        def containsAll(comps1: List[Formula], comps2: List[Formula]): Boolean =
          comps2.forall(fm => comps1.exists(compare(_, fm)))

        (selfComps.size == otherComps.size) && containsAll(selfComps, otherComps) && containsAll(
          otherComps,
          selfComps
        )

      (fm1, fm2) match
        case (or1: Or, or2: Or)     => hasSameComps(or1, or2, Or.flatten)
        case (and1: And, and2: And) => hasSameComps(and1, and2, And.flatten)
        case (iff1: Iff, iff2: Iff) => hasSameComps(iff1, iff2, Iff.flatten)
        case (thisCnf, thatCnf)     => thisCnf == thatCnf

    compare(fm1, fm2)
