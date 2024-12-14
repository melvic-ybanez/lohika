package com.melvic.lohika.core

import BaseSpec.parseFailure
import com.melvic.lohika.core.meta.Entailment
import com.melvic.lohika.core.parsers.Parser
import fastparse.Parsed

class EntailmentSpec extends BaseSpec:
  "Derived entailments" should "unfold given propositional definitions" in:
    assertUnfoldedEquals(
      s"""
        |PremiseA := A:xE:y[P(x, y) -> E:z[!R(z) -> Q(x)]];
        |PremiseB := A:x!Q(x);
        |PremiseC := !R(w);
        |
        |Conclusion := A:aE:b!P(a, b);
        |
        |PremiseA, PremiseB, PremiseC |= Conclusion
        |""".stripMargin,
      "A:xE:y[P(x, y) -> E:z[!R(z) -> Q(x)]], A:x!Q(x), !R(w) |= A:aE:b!P(a, b)"
    )

  it should "unfold given predicate definitions" in:
    assertUnfoldedEquals(
      s"""
        |P(a, b) := Q(b, a);
        |A:x, y[P(x, y)] |= Q(b, c)
        |""".stripMargin,
      "A:x, yQ(y, x) |= Q(b, c)"
    )

  it should "unfold given function definitions" in:
    assertUnfoldedEquals(
      s"""
         |f(x, y) := g(h(y), x);
         |A:a,bP(f(a, b))
         |""".stripMargin,
      "A:a,bP(g(h(b), a))"
    )
    assertUnfoldedEquals(
      s"""
         |f(x, y) := g(h(y), x);
         |R(x, y) := P(y, x);
         |
         |R(s, f('r, 'u)) |= A:a,bP(f(a, b))
         |""".stripMargin,
      "P(g(h('u), 'r), s) |= A:a,bP(g(h(b), a))"
    )

  it should "unfold recursively" in:
    assertUnfoldedEquals(
      s"""
         |P(a, b) := Q(b, a);
         |R(x) := S(x) & A:xS(x) & P(x, x);
         |A:x, y[P(x, y)], R(y) |= Q(b, c)
         |""".stripMargin,
      "A:x, yQ(y, x), S(y) & A:xS(x) & Q(y, y) |= Q(b, c)"
    )
    assertUnfoldedEquals(
      s"""
         |f(x, y) := g(x, y);
         |r(a, b) := w(f(a, a), b);
         |
         |A:s,tP(r(s, t))
         |""".stripMargin,
      "A:s,tP(w(g(s, s), t))"
    )

  "Comments" should "be skipped" in:
    assertUnfoldedEquals(
      s"""
         |# define the premises
         |PremiseA := A:xE:y[P(x, y) -> E:z[!R(z) -> Q(x)]];
         |PremiseB := A:x!Q(x);
         |PremiseC := !R(w);    # `w` is a free variable
         |
         |Conclusion := A:aE:b!P(a, b);
         |PremiseA, PremiseB, PremiseC |= Conclusion   # the entailment to prove
         |""".stripMargin,
      "A:xE:y[P(x, y) -> E:z[!R(z) -> Q(x)]], A:x!Q(x), !R(w) |= A:aE:b!P(a, b)"
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
