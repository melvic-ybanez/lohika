package com.melvic.lohika

import cats.*
import cats.implicits.*
import com.melvic.lohika.Formatter.*
import com.melvic.lohika.formula.Formula

final case class Entailment(premises: List[Formula], conclusion: Formula)

object Entailment:
  given (using formatter: Formatter): Show[Entailment] = Show.show:
    case Entailment(Nil, conclusion)      => conclusion.show
    case Entailment(premises, conclusion) =>
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

      formatter.formula(show"${premises.map(_.show).mkString(", ")} |= $conclusion")
