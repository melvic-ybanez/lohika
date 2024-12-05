package com.melvic.lohika

import com.melvic.lohika.BaseSpec.parseFailure
import com.melvic.lohika.meta.Entailment
import com.melvic.lohika.parsers.Parser
import fastparse.Parsed

class EntailmentSpec extends BaseSpec:
  "Derived entailments" should "unfold given propositional definitions" in:
    assertUnfoldedEquals(
      s"""
        |PremiseA := A:xE:y[P(x, y) -> E:z[!R(z) -> Q(x)]];
        |PremiseB := A:x!Q(x);
        |PremiseC := !R(w);
        |Conclusion := A:aE:b!P(a, b);
        |
        |PremiseA, PremiseB, PremiseC |= Conclusion
        |""".stripMargin.trim,
      "A:xE:y[P(x, y) -> E:z[!R(z) -> Q(x)]], A:x!Q(x), !R(w) |= A:aE:b!P(a, b)"
    )

  it should "unfold given predicate definitions" in:
    assertUnfoldedEquals(
      s"""
        |P(a, b) := Q(b, a);
        |A:x, y[P(x, y)] |= Q(b, c)
        |""".stripMargin.trim,
      "A:x, yQ(y, x) |= Q(b, c)"
    )

  it should "unfold recursively" in:
    assertUnfoldedEquals(
      s"""
         |P(a, b) := Q(b, a);
         |R(x) := S(x) & A:xS(x) & P(x, x);
         |A:x, y[P(x, y)], R(y) |= Q(b, c)
         |""".stripMargin.trim,
      "A:x, yQ(y, x), S(y) & A:xS(x) & Q(y, y) |= Q(b, c)"
    )

  def parse(input: String): Option[Entailment] =
    Parser.parseEntailment(input) match
      case Parsed.Success(entailment, _) => Some(entailment)
      case _                             => None

  def assertParseEquals(entailment1: Entailment, rawEntailment2: String): Unit =
    parse(rawEntailment2).fold(parseFailure(rawEntailment2, "Second entailment")): entailment2 =>
      entailment1 should be(entailment2)

  def assertUnfoldedEquals(rawEntailment1: String, rawEntailment2: String): Unit =
    parse(rawEntailment1)
      .map(Entailment.unfold)
      .fold(parseFailure(rawEntailment1, "First entailment")): entailment1 =>
        assertParseEquals(entailment1, rawEntailment2)
