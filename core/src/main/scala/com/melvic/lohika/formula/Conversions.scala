package com.melvic.lohika.formula

import cats.Endo
import com.melvic.lohika.formula.Formula.*

private[formula] trait Conversions extends NnfConversions with AlphaConversion with Standardization:
  type Convert[F <: Formula] = Endo[Formula] => F => Formula
  type Unless = PartialFunction[Formula, Unit]

  class ConvertFormula(formula: Formula):
    def unless(f: Unless): ConvertFormulaUnless =
      ConvertFormulaUnless(formula, f)

    def when(f: PartialFunction[Formula, Unit]): ConvertFormulaUnless =
      ConvertFormulaUnless(formula, Function.unlift(fm => Option.when(!f.isDefinedAt(fm))(())))

    def by(convert: Endo[Formula]): Formula =
      ConvertFormulaUnless(formula, PartialFunction.empty).by(convert)

  class ConvertFormulaUnless(formula: Formula, unless: Unless):
    def by(convert: Endo[Formula]): Formula =
      formula match
        case fm if unless.isDefinedAt(fm) => fm
        case iff: Iff                     => convertBiconditional(convert)(iff)
        case imply: Imply                 => convertImplication(convert)(imply)
        case or: Or                       => convertDisjunction(convert)(or)
        case and: And                     => convertConjunction(convert)(and)
        case not: Not                     => convertNegation(convert)(not)
        case forall: Forall               => convertUniversal(convert)(forall)
        case thereExists: ThereExists     => convertExistential(convert)(thereExists)
        case fm                           => fm

  def eliminateBiconditionals: Endo[Formula] =
    case Iff(p, q, Nil) => eliminateBiconditionals((p ==> q) & (q ==> p))
    case Iff(p, q, rs) =>
      val iffs = rs.foldLeft(List(p <==> q)):
        case (iffs @ (Iff(_, q, _) :: _), r) => (q <==> r) :: iffs
      eliminateBiconditionals(And.fromList(iffs.reverse))
    case fm => convert(fm).unless { case _: Iff => }.by(eliminateBiconditionals)

  def eliminateImplications: Endo[Formula] =
    case Imply(p, q) => eliminateImplications(!p | q)
    case fm          => convert(fm).unless { case _: Iff | _: Imply => }.by(eliminateImplications)

  def distributeOrOverAnds: Endo[Formula] =
    case Or(p, And(ap, aq, ars), Nil) =>
      convertConjunction(distributeOrOverAnds)(And(p | ap, p | aq, ars.map(p | _)))
    case Or(And(ap, aq, ars), q, Nil) =>
      convertConjunction(distributeOrOverAnds)(And(ap | q, aq | q, ars.map(_ | q)))
    case Or(p, and: And, r :: rs) => distributeOrOverAnds(Or(distributeOrOverAnds(p | and), r, rs))
    case Or(and: And, q, r :: rs) => distributeOrOverAnds(Or(distributeOrOverAnds(and | q), r, rs))
    case Or(p, q, (and: And) :: rs) =>
      distributeOrOverAnds(Or(p, distributeOrOverAnds(q | and), rs))
    case Or(p, q, r :: rs) => distributeOrOverAnds(p | distributeOrOverAnds(Or(q, r, rs)))
    case fm                => convert(fm).when { case _: And | _: Not => }.by(distributeOrOverAnds)

  def flattenConjunctions: Endo[Formula] =
    case and: And =>
      val flattened = and.components.map(flattenOrsAndAnds)
      flattened.tail.foldLeft(flattened.head):
        case (And(p, q, rs), and: And) => And(p, q, rs ++ and.components)
        case (And(p, q, rs), fm)       => And(p, q, rs ++ List(fm))
        case (fm, And(p, q, rs))       => And(fm, p, q :: rs)
        case (fm1, fm2)                => fm1 & fm2
    case or: Or => convertDisjunction(flattenConjunctions)(or)
    case fm     => fm

  def flattenDisjunctions: Endo[Formula] =
    case or: Or =>
      val flattened = or.components.map(flattenOrsAndAnds)
      flattened.tail.foldLeft(flattened.head):
        case (Or(p, q, rs), or: Or) => Or(p, q, rs ++ or.components)
        case (Or(p, q, rs), fm)     => Or(p, q, rs ++ List(fm))
        case (fm, Or(p, q, rs))     => Or(fm, p, q :: rs)
        case (fm1, fm2)             => fm1 | fm2
    case and: And => convertConjunction(flattenDisjunctions)(and)
    case fm       => fm

  def flattenOrsAndAnds: Endo[Formula] =
    case and: And => flattenConjunctions(and)
    case or: Or   => flattenDisjunctions(or)
    case fm       => fm

  def convert(formula: Formula): ConvertFormula = ConvertFormula(formula)

  def convertBiconditional: Convert[Iff] = f =>
    case Iff(p, q, rs) => Iff(f(p), f(q), rs.map(f))

  def convertImplication: Convert[Imply] = f =>
    case Imply(p, q) => f(p) ==> f(q)

  def convertDisjunction: Convert[Or] = f =>
    case Or(p, q, rs) => Or(f(p), f(q), rs.map(f))

  def convertConjunction: Convert[And] = f =>
    case And(p, q, rs) => And(f(p), f(q), rs.map(f))

  def convertNegation: Convert[Not] = f =>
    case Not(p) => Not(f(p))

  def convertUniversal: Convert[Forall] = f =>
    case Forall(boundVars, matrix) => Forall(boundVars, f(matrix))

  def convertExistential: Convert[ThereExists] = f =>
    case ThereExists(boundVars, matrix) => ThereExists(boundVars, f(matrix))
