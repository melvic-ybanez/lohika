package com.melvic.lohika.core.formula

import cats.*
import cats.implicits.*
import com.melvic.lohika.core.Givens
import com.melvic.lohika.core.expression.Expression.*
import com.melvic.lohika.core.formula.Formula.*
import com.melvic.lohika.core.formula.Formula.Quantified.{forall, thereExists}
import com.melvic.lohika.core.parsers.Parser
import fastparse.Parsed
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class PrettyPrintSpec extends AnyFlatSpec with should.Matchers with Givens:
  "A variable" should "print its name" in:
    Var("A").show should be("A")

  "A constant" should "have the correct prefix" in:
    Const("x").show should be("'x")

  "Disjunctions" should "be separated by |s" in:
    Or.of("A", "B", "C").show should be("A | B | C")

  it should "wrapped components with lower or equal precedence levels in parens" in:
    Or.of("A", "B" | "C", "D").show should be("A | (B | C) | D")
    Or.of("A", "B" --> "C", "D", "E" <--> "F").show should be(
      "A | (B -> C) | D | (E <-> F)"
    )

  it should "not wrapped components with higher precedence levels in parens" in:
    Or.of("A", "B" & "C", "D").show should be("A | B & C | D")
    Or.of("A", !"B", "D", !"E").show should be("A | !B | D | !E")

  "Conjunctions" should "be separated by &s" in:
    And.of("A", "B", "C").show should be("A & B & C")

  it should "wrapped components with lower or equal precedence levels in parens" in:
    And.of("A", "B" & "C", "D").show should be("A & (B & C) & D")
    And.of("A", "B", "C" | "D").show should be("A & B & (C | D)")
    And.of("A", "B" --> "C", "D", "E" <--> "F").show should be(
      "A & (B -> C) & D & (E <-> F)"
    )
    ("A" | "B" --> "C").show should be("A | (B -> C)")

  it should "not wrapped components with higher precedence levels in parens" in:
    And.of("A", !"B", "D", !"E").show should be("A & !B & D & !E")

  "Implications" should "be separated by `->`s" in:
    ("A" --> "B").show should be("A -> B")
    Imply.of("A", "B", "C").show should be("A -> B -> C")

  it should "wrapped components with lower or equal precedence levels in parens" in:
    Imply.of("A" --> "B", "C").show should be("(A -> B) -> C")
    Imply.of("A", "B" --> "C", "D").show should be("A -> (B -> C) -> D")
    Imply.of("A", "B" <--> "C", "D", "E" <--> "F").show should be(
      "A -> (B <-> C) -> D -> (E <-> F)"
    )

  it should "not wrapped components with higher precedence levels in parens" in:
    (("A" | "B") --> "C").show should be("A | B -> C")
    ("A" --> ("B" & "C")).show should be("A -> B & C")
    Imply.of("A", !"B", "D", !"E").show should be("A -> !B -> D -> !E")

  "Biconditionals" should "be separated by `<->`s" in:
    ("A" <--> "B").show should be("A <-> B")

  "Universal Quantification" should "start with `A:`" in:
    forall("x", "y")("P" --> "Q").show should be("A:x,y[P -> Q]")
    forall("x")(forall("y")(("A" --> "B") & "C")).show should be("A:xA:y[(A -> B) & C]")

  "Existential Quantification" should "start with `E:`" in:
    thereExists("x", "y")("P" --> "Q").show should be("E:x,y[P -> Q]")
    thereExists("x")(thereExists("y")(("A" --> "B") & "C")).show should be("E:xE:y[(A -> B) & C]")

  "Predicates application" should "look like function calls" in:
    "P".of("x").show should be("P(x)")
    thereExists("x", "y")("P".of("x", "y") --> "Q".of("y")).show should be(
      "E:x,y[P(x, y) -> Q(y)]"
    )

  "Function calls" should "look like predicates with lower case names" in:
    assertParsePrettifyDefault("A:xP(x, f(x))")
    assertParsePrettifyDefault("A:x[P(x, f(x)) & Q(f(x), g(x))]")
    assertParsePrettifyDefault("A:x,yA:a[P(a) & Q(f(x, y, a))]")

  "Quantified formulas" should "print brackets around the matrix only if necessary" in:
    forall("x", "y")("P".of("x") --> "Q".of("y")).show should be("A:x,y[P(x) -> Q(y)]")
    assertParsePrettifyDefault("A:x,y[P(x) -> Q(y)]")
    assertParsePrettifyDefault("A:xP(x)")
    assertParsePrettify("A:x!P(x)", "A:x[!P(x)]")
    assertParsePrettify("A:x(E:y(P(x) -> Q(y)))", "A:xE:y[P(x) -> Q(y)]")
    assertParsePrettify("A:xE:y(P(x) -> Q(y))", "A:xE:y[P(x) -> Q(y)]")

  def assertParsePrettifyDefault(input: String): Unit =
    assertParsePrettify(input, input)

  def assertParsePrettify(input: String, pretty: String): Unit =
    Parser.parseFormula(input) match
      case Parsed.Success(formula, _)  => formula.show should be(pretty)
      case Parsed.Failure(label, _, _) => assert(false, s"Unable to parse $input. Details: $label")
