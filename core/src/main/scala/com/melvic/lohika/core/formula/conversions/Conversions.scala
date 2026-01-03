package com.melvic.lohika.core.formula.conversions

import cats.Endo
import cats.data.State
import cats.implicits.*
import com.melvic.lohika.core.formula.Formula
import Formula.*
import com.melvic.lohika.core.rewriteOrId

private[formula] trait Conversions
    extends CnfConversion
    with NnfConversion
    with AlphaConversion
    with Standardization
    with PnfConversion
    with Skolemization:
  type Convert[F <: Formula] = Endo[Formula] => F => Formula

  private[formula] final case class NoIff(raw: Formula)
  private[formula] final case class NoIf(raw: Formula)
  private[formula] final case class DistributedOrs(raw: Formula)
  private[formula] final case class QuantifierFree(raw: Formula)

  def eliminateBiconditionals: Formula => NoIff =
    def recurse: Endo[Formula] =
      case Iff(p, q, Nil) => recurse((p --> q) & (q --> p))
      case Iff(p, q, rs) =>
        val iffs = rs.foldLeft(List(p <--> q)):
          case (iffs @ (Iff(_, q, _) :: _), r) => (q <--> r) :: iffs
        recurse(And.fromList(iffs.reverse))
      case fm => convertBy(recurse)(fm)

    fm => NoIff(recurse(fm))

  def eliminateImplications: NoIff => NoIf =
    case NoIff(fm) =>
      def recurse: Endo[Formula] =
        case Imply(p, q) => recurse(!p | q)
        case fm          => convertBy(recurse)(fm)

      NoIf(recurse(fm))

  def distributeOrOverAnds: QuantifierFree => DistributedOrs =
    def recurse: Endo[Formula] =
      case Or(p, And(ap, aq, ars), Nil) =>
        convertConjunction(recurse)(And(p | ap, p | aq, ars.map(p | _)))
      case Or(And(ap, aq, ars), q, Nil) =>
        convertConjunction(recurse)(And(ap | q, aq | q, ars.map(_ | q)))
      case Or(p, and: And, r :: rs) => recurse(Or(recurse(p | and), r, rs))
      case Or(and: And, q, r :: rs) => recurse(Or(recurse(and | q), r, rs))
      case Or(p, q, (and: And) :: rs) =>
        recurse(Or(p, recurse(q | and), rs))
      case Or(p, q, r :: rs) => recurse(p | recurse(Or(q, r, rs)))
      case fm                => convertBy(recurse)(fm)

    matrixForm => DistributedOrs(recurse(matrixForm.raw))

  private def flattenConjunctionsRaw: Endo[Formula] = rewriteOrId:
    case and: And =>
      val flattened = and.components.map(flattenOrsAndAndsRaw)
      flattened.tail.foldLeft(flattened.head):
        case (And(p, q, rs), and: And) => And(p, q, rs ++ and.components)
        case (And(p, q, rs), fm)       => And(p, q, rs ++ List(fm))
        case (fm, And(p, q, rs))       => And(fm, p, q :: rs)
        case (fm1, fm2)                => fm1 & fm2
    case or: Or => convertDisjunction(flattenConjunctionsRaw)(or)

  private def flattenDisjunctionsRaw: Endo[Formula] = rewriteOrId:
    case or: Or =>
      val flattened = or.components.map(flattenOrsAndAndsRaw)
      flattened.tail.foldLeft(flattened.head):
        case (Or(p, q, rs), or: Or) => Or(p, q, rs ++ or.components)
        case (Or(p, q, rs), fm)     => Or(p, q, rs ++ List(fm))
        case (fm, Or(p, q, rs))     => Or(fm, p, q :: rs)
        case (fm1, fm2)             => fm1 | fm2
    case and: And => convertConjunction(flattenDisjunctionsRaw)(and)

  private def flattenOrsAndAndsRaw: Endo[Formula] = rewriteOrId:
    case and: And => flattenConjunctionsRaw(and)
    case or: Or   => flattenDisjunctionsRaw(or)

  def flattenOrsAndAnds(distributedOrs: DistributedOrs): Formula =
    flattenOrsAndAndsRaw(distributedOrs.raw)

  def convertBy(transform: Endo[Formula]): Endo[Formula] = rewriteOrId:
    case iff: Iff                 => convertBiconditional(transform)(iff)
    case imply: Imply             => convertImplication(transform)(imply)
    case or: Or                   => convertDisjunction(transform)(or)
    case and: And                 => convertConjunction(transform)(and)
    case not: Not                 => convertNegation(transform)(not)
    case forall: Forall           => convertUniversal(transform)(forall)
    case thereExists: ThereExists => convertExistential(transform)(thereExists)

  def convertBiconditional: Convert[Iff] = f =>
    case Iff(p, q, rs) => Iff(f(p), f(q), rs.map(f))

  def convertImplication: Convert[Imply] = f =>
    case Imply(p, q) => f(p) --> f(q)

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

  def convertFListM[S](fList: FList)(by: Formula => State[S, Formula]): State[S, FList.Args] =
    for
      sp  <- by(fList.p)
      sq  <- by(fList.q)
      srs <- fList.rs.map(by).sequence
    yield (sp, sq, srs)

  def dropUniversalQuantifiers: Snf => QuantifierFree =
    case Snf(Forall(boundVars, scope)) => dropUniversalQuantifiers(Snf(scope))
    case Snf(scope)                    => QuantifierFree(scope)
