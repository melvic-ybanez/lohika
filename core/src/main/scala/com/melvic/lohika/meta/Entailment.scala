package com.melvic.lohika.meta

import cats.*
import cats.implicits.*
import com.melvic.lohika.Formatter.*
import com.melvic.lohika.formula.Formula
import com.melvic.lohika.Formatter
import com.melvic.lohika.parsers.Lexemes

final case class Entailment(
    definitions: List[Definition],
    premises: List[Formula],
    conclusion: Formula
):
  def toDirect: Entailment =
    this.copy(definitions = Nil)

object Entailment:
  def direct(premises: List[Formula], conclusion: Formula): Entailment =
    Entailment(Nil, premises, conclusion)

  given (using formatter: Formatter): Show[List[Definition]] = Show.show:
    _.map(_.show).mkString(Lexemes.StmtDelimiter + "\n\n")

  given (using formatter: Formatter): Show[Entailment] = Show.show:
    case Entailment(Nil, Nil, conclusion) => conclusion.show
    case Entailment(definitions, Nil, conclusion) =>
      show"$definitions${Lexemes.StmtDelimiter}\n\n$conclusion"
    case Entailment(Nil, premises, conclusion) =>
      // Disable the formatter for the formula so that it won't
      // accidentally format the premises and the conclusion separately.
      // Otherwise, it will screw up the rendering (due to extra html tags).
      // We will use the formula formatter only for the whole entailment.
      given Formatter with
        def emphasize: Format = formatter.emphasize

        def strong: Format = formatter.strong

        def link(target: String): Format = formatter.link(target)

        def itemNumber: String = formatter.itemNumber

        override def formula: Format = identity

      formatter.formula(
        show"${premises.map(_.show).mkString(", ")} ${Lexemes.Entailment} $conclusion"
      )
    case entailment @ Entailment(definitions, _, _) =>
      show"$definitions${Lexemes.StmtDelimiter}\n\n${entailment.toDirect}"
