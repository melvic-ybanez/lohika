package com.melvic.lohika.tests

import com.melvic.lohika.formula.Formula
import com.melvic.lohika.formula.Formula.*
import com.melvic.lohika.formula.Formula.Quantified.{thereExists, forall}
import com.melvic.lohika.parsers.Parser
import fastparse.Parsed

class FormulaParserSpec extends BaseSpec:
  "True" should "be written as T" in:
    parseSuccess("T", True)

  "False" should "be written as F" in:
    parseSuccess("F", False)

  "A single alphanumeric string" should "map to a variable" in:
    parseSuccess("A", Var("A"))

  it should "support multiple characters" in:
    parseSuccess("Foo", Var("Foo"))

  "Negation" should "start with !" in:
    parseSuccess("!P", !"P")
    parseSuccess("!!A", Not(Not("A")))

  it should "have higher precedence than Conjunction" in:
    parseSuccess("!A & !B", !"A" & !"B")
    parseSuccess("!(A & B)", !("A" & "B"))

  "Disjunctions" should "be separated by |" in:
    parseSuccess("A | B", "A" | "B")
    parseSuccess("some | where", "some" | "where")

  it should "support chaining" in:
    parseSuccess("A|B|C", Or.of("A", "B", "C"))

  it should "accept lower precedence components if they are inside parens" in:
    parseSuccess("(A => B) | C", ("A" ==> "B") | "C")
    parseSuccess("(A <=> B) | C", ("A" <==> "B") | "C")

  "Conjunctions" should "be separated by &" in:
    parseSuccess("A & B", "A" & "B")
    parseSuccess("some & where", "some" & "where")

  it should "support chaining" in:
    parseSuccess("A&B&C", And.of("A", "B", "C"))

  it should "have higher precedence than disjunction" in:
    parseSuccess("A | B & C", "A" | ("B" & "C"))

  it should "accept lower precedence components if they are inside parens" in:
    parseSuccess("(A | B) & C", ("A" | "B") & "C")
    parseSuccess("(A => B) & C", ("A" ==> "B") & "C")
    parseSuccess("(A <=> B) & C", ("A" <==> "B") & "C")
    parseSuccess("(A | B) & (C | D)", ("A" | "B") & ("C" | "D"))

  "Implication" should "be connected by =>" in:
    parseSuccess("A => B", "A" ==> "B")

  it should "support grouping by parenthesis" in:
    parseSuccess("(A => B) => C", ("A" ==> "B") ==> "C")
    parseSuccess("A => (B => C) => D", "A" ==> (("B" ==> "C") ==> "D"))

  it should "be right associative" in:
    parseSuccess("A => B => C", "A" ==> ("B" ==> "C"))
    "A => (B => C)" === "A => B => C"

  it should "have lower precedence than disjunction" in:
    parseSuccess("A | B => C | D", ("A" | "B") ==> ("C" | "D"))

  it should "accept lower precedence components if they are inside parens" in:
    parseSuccess("(A <=> B) => C", ("A" <==> "B") ==> "C")

  "Biconditional" should "be connected by <=>" in:
    parseSuccess("A <=> B", "A" <==> "B")

  it should "have lower precedence than implication" in:
    parseSuccess("A <=> B => C", "A" <==> ("B" ==> "C"))

  it should "support chaining" in:
    parseSuccess("A <=> B <=> C", Iff.of("A", "B", "C"))

  "Forall" should "take a set of first-order variables and a matrix" in:
    parseSuccess("A:x,y(P => Q)", forall("x", "y")("P" ==> "Q"))
    parseSuccess("A:x(Q)", forall("x")("Q"))
    parseSuccess("A:xP(x)", forall("x")("P".of("x")))
    parseSuccess("A:xE:y(P(x) => Q(y))", forall("x")(thereExists("y")("P".of("x") ==> "Q".of("y"))))
    parseSuccess("A:xE:yP(x) => Q(y)", forall("x")(thereExists("y")("P".of("x"))) ==> "Q".of("y"))
    parseFailure("A:(P => Q)")
    parseFailure("A:X(P => Q)")

  "Exists" should "take a set of variables and a matrix" in:
    parseSuccess("E:x,y(P => Q)", thereExists("x", "y")("P" ==> "Q"))
    parseSuccess("E:x(Q)", thereExists("x")("Q"))
    parseSuccess("E:xP(x)", thereExists("x")("P".of("x")))
    parseSuccess("E:xA:y(P(x) => Q(y))", thereExists("x")(forall("y")("P".of("x") ==> "Q".of("y"))))
    parseFailure("E:(P => Q)")
    parseFailure("E:X(P => Q)")

  "Predicates" should "be able to take arguments" in:
    parseSuccess("P(x)", "P".of("x"))
    parseSuccess("E:x,y(P(x, y) => Q(y))", thereExists("x", "y")("P".of("x", "y") ==> "Q".of("y")))

  it should "only take first-order variables as arguments" in:
    parseFailure("P(X)")

  def parseSuccess(input: String, expected: Formula): Unit =
    Parser.parseFormula(input) should matchPattern:
      case Parsed.Success(`expected`, _) =>

  def parseFailure(input: String): Unit =
    Parser.parseFormula(input) should matchPattern:
      case Parsed.Failure(_, _, _) =>
