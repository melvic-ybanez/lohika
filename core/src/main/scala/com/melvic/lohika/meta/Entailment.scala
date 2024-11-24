package com.melvic.lohika.meta

import cats.*
import cats.data.NonEmptyList
import cats.implicits.*
import com.melvic.lohika.Formatter.*
import com.melvic.lohika.formula.Formula
import com.melvic.lohika.Formatter
import com.melvic.lohika.meta.Definition.FormulaDef
import com.melvic.lohika.meta.Entailment.{Derived, Direct}
import com.melvic.lohika.parsers.Lexemes

type Entailment = Derived | Direct

object Entailment:
  final case class Derived(
      definitions: NonEmptyList[Definition],
      premises: List[Formula],
      conclusion: Formula
  ):
    val toDirect: Direct = Direct(premises, conclusion)

  final case class Direct(premises: List[Formula], conclusion: Formula)

  def unapply(entailment: Entailment): (List[Definition], List[Formula], Formula) =
    entailment match
      case Direct(premises, conclusion)               => (Nil, premises, conclusion)
      case Derived(definitions, premises, conclusion) => (definitions.toList, premises, conclusion)

  def unfold: Entailment => Direct =
    case direct: Direct => direct
    case Derived(definitions, premises, conclusion) =>
      given definitionList: List[FormulaDef] = definitions.toList.collect:
        case fm: FormulaDef => fm
      Direct(premises.map(Formula.unfold), Formula.unfold(conclusion))

  given [E <: Entailment](using formatter: Formatter): Show[E] = Show.show:
    case Direct(Nil, conclusion) => conclusion.show
    case Derived(definitions, Nil, conclusion) =>
      show"$definitions${Lexemes.StmtDelimiter}${formatter.newline}$conclusion"
    case Direct(premises, conclusion) =>
      // Disable the formatter for the formula so that it won't
      // accidentally format the premises and the conclusion separately.
      // Otherwise, it will screw up the rendering (due to extra html tags).
      // We will use the formula formatter only for the whole entailment.
      given Formatter with
        def emphasize: Format = formatter.emphasize

        def strong: Format = formatter.strong

        def link(target: String): Format = formatter.link(target)

        def itemNumber: String = formatter.itemNumber

        def newline: String = formatter.newline

        override def formula: Format = identity

      formatter.formula(
        show"${premises.map(_.show).mkString(", ")} ${Lexemes.Entailment} $conclusion"
      )
    case entailment @ Derived(definitions, _, _) =>
      show"$definitions${Lexemes.StmtDelimiter}${formatter.newline}${entailment.toDirect}"
