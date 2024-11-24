package com.melvic.lohika

import cats.data.NonEmptyList
import com.melvic.lohika.formula.Formula.*
import com.melvic.lohika.formula.Formula.PredicateApp.nullary
import com.melvic.lohika.meta.Entailment.Derived
import com.melvic.lohika.meta.{Definition, Entailment}
import com.melvic.lohika.parsers.Parser
import fastparse.Parsed

class EntailmentParserSpec extends BaseSpec:
  "Entailments" should "support definitions" in:
    parseSuccess(
      "P <- R & T; Q <- X; P -> Q",
      Derived(
        NonEmptyList.of(
          Definition(nullary("P"), "R" & "T"),
          Definition(nullary("Q"), "X")
        ),
        Nil,
        "P" --> "Q"
      )
    )

    parseSuccess(
      s"""
         |P <- R & T;
         |Q <- X;
         |P | Q |= P -> Q
         |""".stripMargin.trim,
      Derived(
        NonEmptyList.of(
          Definition(nullary("P"), "R" & "T"),
          Definition(nullary("Q"), "X")
        ),
        List(nullary("P") | nullary("Q")),
        "P" --> "Q"
      )
    )

  def parseSuccess(input: String, expected: Entailment): Unit =
    Parser.parseEntailment(input) should matchPattern:
      case Parsed.Success(`expected`, _) =>
