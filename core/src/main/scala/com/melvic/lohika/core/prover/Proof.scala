package com.melvic.lohika.core.prover

import cats.implicits.showInterpolator
import com.melvic.lohika.core.Formatter.{emphasize, link, sentence, strong}
import com.melvic.lohika.core.formula.Cnf.{CAnd, CNot, Clause}
import com.melvic.lohika.core.formula.Formula.PredicateApp
import com.melvic.lohika.core.formula.{Cnf, Formula}
import com.melvic.lohika.core.parsers.Lexemes
import com.melvic.lohika.core.prover.Proof.Step.*
import com.melvic.lohika.core.prover.Proof.{Binary, Nullary, Unary}
import com.melvic.lohika.core.{Formatter, Links}
import com.melvic.lohika.core.Givens.given

import scala.collection.immutable.ListSet

type Proof = Nullary | Unary | Binary

object Proof:
  final case class Nullary(conclusion: Step)
  final case class Unary(
      conclusion: Identity | Rewrite | Exhaustion.type | Conclusion | AndElim,
      proof: Proof
  )
  final case class Binary(
      conclusion: Derived | Contradiction,
      leftProof: Proof,
      rightProof: Proof
  )

  def toSteps: Proof => ListSet[Step] =
    case Unary(conclusion, proof) => toSteps(proof) ++ ListSet(conclusion)
    case Binary(conclusion, leftProof, rightProof) =>
      toSteps(leftProof) ++ toSteps(rightProof) ++ ListSet(conclusion)
    case Nullary(step) => ListSet(step)

  def toProse(using Formatter): Proof => ListSet[String] =
    toSteps.andThen:
      _.filter:
        case Identity(_) => false
        case _           => true
      .map:
        case Conversion(from, to)             => show"Rewriting $from, we get $to"
        case ConclusionNeg(negatedConclusion) => show"Supposed $negatedConclusion"
        case AndElim(from, to)                => show"From $from, we get $to"
        case Derived(p, q, r)                 => show"Since $p and $q, it follows that $r"
        case Contradiction(p, q) =>
          show"We have both $p and $q. This is a ${"contradiction".emphasize}"
        case Exhaustion => s"Resolution options ${"exhausted".emphasize}"
        case Conclusion(premises, conclusion, provable) =>
          val followsString = if provable then "follows" else "does not follow"
          val notString = if provable then "" else " not"
          val therefore = "Therefore, "

          if premises.isEmpty then
            show"$therefore$conclusion is$notString a ${"tautology".link(Links.Tautology)}"
          else show"$therefore$conclusion ${followsString.strong} from $premises"
        case Step.Identity(_) => ""
      .map(_.sentence)

  type Step = Identity | Rewrite | AndElim | ResolutionResult | Conclusion

  object Step:
    type Rewrite = Conversion | ConclusionNeg
    type ResolutionResult = Derived | Contradiction | Exhaustion.type

    final case class Identity(formula: Cnf)

    final case class Conversion(from: Formula, to: Cnf)

    final case class AndElim(from: CAnd, to: Clause)

    final case class ConclusionNeg(negatedConclusion: Formula)

    final case class Derived(p: Clause, q: Clause, r: Clause)

    final case class Contradiction(p: Clause, q: Clause)

    case object Exhaustion

    final case class Conclusion(premises: List[Formula], conclusion: Formula, provable: Boolean)

    object Contradiction:
      /**
       * Creates a contradiction (e.g. P & !P) from the variable name. If the input is in the form
       * `!P`, the resulting contradiction is `!P & P`. Otherwise, it is `P & !P`
       */
      def fromPropVarName(varName: String): Contradiction =
        if varName.startsWith(Lexemes.Not) then
          val variable = PredicateApp.nullary(varName.tail)
          Contradiction(CNot(variable), variable)
        else
          val variable = PredicateApp.nullary(varName)
          Contradiction(variable, CNot(variable))

    object Conclusion:
      def provable(using premises: List[Formula], conclusion: Formula): Conclusion =
        Conclusion(premises, conclusion, true)

      def notProvable(using premises: List[Formula], conclusion: Formula): Conclusion =
        Conclusion(premises, conclusion, false)
