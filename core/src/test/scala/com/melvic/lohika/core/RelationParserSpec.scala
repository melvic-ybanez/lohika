package com.melvic.lohika.core

import cats.data.NonEmptyList
import com.melvic.lohika.core.formula.Formula.*
import com.melvic.lohika.core.formula.Formula.PredicateApp.nullary
import com.melvic.lohika.core.meta.Definition.{FormulaDef, PropId}
import com.melvic.lohika.core.meta.Entailment.Derived
import com.melvic.lohika.core.meta.{Definition, Entailment}
import com.melvic.lohika.core.parsers.Parser
import fastparse.Parsed

class RelationParserSpec extends BaseSpec:
  "Entailments" should "support formula definitions" in:
    parseSuccess(
      "P := R & T; Q := X; P -> Q",
      Derived(
        NonEmptyList.of(
          FormulaDef(PropId("P"), "R" & "T"),
          FormulaDef(PropId("Q"), "X")
        ),
        Nil,
        "P" --> "Q"
      )
    )

    parseSuccess(
      s"""
         |P := R & T;
         |Q := X;
         |P | Q |= P -> Q
         |""".stripMargin,
      Derived(
        NonEmptyList.of(
          FormulaDef(PropId("P"), "R" & "T"),
          FormulaDef(PropId("Q"), "X")
        ),
        List(nullary("P") | nullary("Q")),
        "P" --> "Q"
      )
    )

  "Equivalences" should "support formula definitions" in:
    parseSuccess(
      "P := R & T; Q := X; P = Q",
      Derived(
        NonEmptyList.of(
          FormulaDef(PropId("P"), "R" & "T"),
          FormulaDef(PropId("Q"), "X")
        ),
        Nil,
        ("P" --> "Q") & ("Q" --> "P")
      )
    )

  def parseSuccess(input: String, expected: Entailment): Unit =
    Parser.parseRelation(input) should matchPattern:
      case Parsed.Success(`expected`, _) =>
