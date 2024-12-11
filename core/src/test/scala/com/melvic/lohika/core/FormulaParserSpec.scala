package com.melvic.lohika.core

import com.melvic.lohika.core.expression.Expression.*
import com.melvic.lohika.core.formula.Formula
import com.melvic.lohika.core.formula.Formula.*
import com.melvic.lohika.core.formula.Formula.Quantified.{forall, thereExists}
import com.melvic.lohika.core.parsers.Parser
import fastparse.Parsed

class FormulaParserSpec extends BaseSpec:
  "A term" should "not be usable outside of atomic formulas" in:
    parseFailure("true")
    parseFailure("x")
    parseSuccess("P(x)", "P".of("x"))

  "True" should "be written as true" in:
    parseSuccess("P(true)", PredicateApp.unary("P", True))

  "False" should "be written as false" in:
    parseSuccess("P(false)", PredicateApp.unary("P", False))

  "A single alphabetic string that starts with an lowercase letter" should "map to first-order variables" in:
    parseSuccess("P(a)", "P".of("a"))

  it should "support multiple characters" in:
    parseSuccess("P(foo)", "P".of("foo"))

  "A constant" should "be written with the correct character as the prefix" in:
    parseSuccess("P('x, 'y)", "P".of(Const("x"), Const("y")))

  "A single alphabetic string that starts with an uppercase letter" should "map to a nullary predicate" in:
    parseSuccess("A", PredicateApp.nullary("A"))

  it should "support multiple characters" in:
    parseSuccess("Foo", PredicateApp.nullary("Foo"))

  "Negation" should "start with !" in:
    parseSuccess("!P", !"P")
    parseSuccess("!!A", Not(Not("A")))

  it should "have higher precedence than Conjunction" in:
    parseSuccess("!A & !B", !"A" & !"B")
    parseSuccess("!(A & B)", !("A" & "B"))

  "Disjunctions" should "be separated by |" in:
    parseSuccess("A | B", "A" | "B")
    parseSuccess("P(a) | !P(a)", "P(a)" | "!P(a)")
    parseSuccess("P(some) | Q(where)", "P(some)" | "Q(where)")

  it should "support chaining" in:
    parseSuccess("A|B|C", Or.of("A", "B", "C"))

  it should "accept lower precedence components if they are inside parens" in:
    parseSuccess("(A -> B) | C", ("A" --> "B") | "C")
    parseSuccess("(A <-> B) | C", ("A" <--> "B") | "C")

  "Conjunctions" should "be separated by &" in:
    parseSuccess("A & B", "A" & "B")
    parseSuccess("P(some) & Q(where)", "P(some)" & "Q(where)")

  it should "support chaining" in:
    parseSuccess("A&B&C", And.of("A", "B", "C"))

  it should "have higher precedence than disjunction" in:
    parseSuccess("A | B & C", "A" | ("B" & "C"))

  it should "accept lower precedence components if they are inside parens" in:
    parseSuccess("(A | B) & C", ("A" | "B") & "C")
    parseSuccess("(A -> B) & C", ("A" --> "B") & "C")
    parseSuccess("(A <-> B) & C", ("A" <--> "B") & "C")
    parseSuccess("(A | B) & (C | D)", ("A" | "B") & ("C" | "D"))

  it should "accept quantified constituents" in:
    parseSuccess(
      "E:x(P(x) | A:yQ(x, y)) & R(x, z)",
      thereExists("x")("P".of("x") | forall("y")("Q".of("x", "y"))) & "R".of("x", "z")
    )

  "Implication" should "be connected by ->" in:
    parseSuccess("A -> B", "A" --> "B")

  it should "support grouping by parenthesis" in:
    parseSuccess("(A -> B) -> C", ("A" --> "B") --> "C")
    parseSuccess("A -> (B -> C) -> D", "A" --> (("B" --> "C") --> "D"))

  it should "be right associative" in:
    parseSuccess("A -> B -> C", "A" --> ("B" --> "C"))
    "A -> (B -> C)" === "A -> B -> C"

  it should "have lower precedence than disjunction" in:
    parseSuccess("A | B -> C | D", ("A" | "B") --> ("C" | "D"))

  it should "accept lower precedence components if they are inside parens" in:
    parseSuccess("(A <-> B) -> C", ("A" <--> "B") --> "C")

  "Biconditional" should "be connected by <->" in:
    parseSuccess("A <-> B", "A" <--> "B")

  it should "have lower precedence than implication" in:
    parseSuccess("A <-> B -> C", "A" <--> ("B" --> "C"))

  it should "support chaining" in:
    parseSuccess("A <-> B <-> C", Iff.of("A", "B", "C"))

  "Forall" should "take a set of first-order variables and a matrix" in:
    parseSuccess("A:x,y(P -> Q)", forall("x", "y")("P" --> "Q"))
    parseSuccess("A:x(Q)", forall("x")("Q"))
    parseSuccess("A:xP(x)", forall("x")("P".of("x")))
    parseSuccess("A:xE:y(P(x) -> Q(y))", forall("x")(thereExists("y")("P".of("x") --> "Q".of("y"))))
    parseSuccess("A:xE:yP(x) -> Q(y)", forall("x")(thereExists("y")("P".of("x"))) --> "Q".of("y"))
    parseFailure("A:(P -> Q)")
    parseFailure("A:X(P -> Q)")

  "Exists" should "take a set of variables and a matrix" in:
    parseSuccess("E:x,y(P -> Q)", thereExists("x", "y")("P" --> "Q"))
    parseSuccess("E:x(Q)", thereExists("x")("Q"))
    parseSuccess("E:xP(x)", thereExists("x")("P".of("x")))
    parseSuccess("E:xA:y(P(x) -> Q(y))", thereExists("x")(forall("y")("P".of("x") --> "Q".of("y"))))
    parseFailure("E:(P -> Q)")
    parseFailure("E:X(P -> Q)")

  "Predicates" should "be able to take arguments" in:
    parseSuccess("P(x)", "P".of("x"))
    parseSuccess("E:x,y(P(x, y) -> Q(y))", thereExists("x", "y")("P".of("x", "y") --> "Q".of("y")))

  it should "only take terms as arguments" in:
    parseFailure("P(X)")
    parseSuccess("P(f(x, y))", "P".of(FunctionApp("f", List(Var("x"), Var("y")))))

  def parseSuccess(input: String, expected: Formula): Unit =
    Parser.parseFormula(input) should matchPattern:
      case Parsed.Success(`expected`, _) =>

  def parseFailure(input: String): Unit =
    Parser.parseFormula(input) should matchPattern:
      case Parsed.Failure(_, _, _) =>
