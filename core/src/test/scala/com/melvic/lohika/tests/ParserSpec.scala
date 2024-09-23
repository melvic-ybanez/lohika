package com.melvic.lohika.tests

import com.melvic.lohika.formula.Formula
import com.melvic.lohika.formula.Formula.*
import com.melvic.lohika.parsers.Parser
import fastparse.Parsed

class ParserSpec extends BaseSpec:
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

  def parseSuccess(input: String, expected: Formula): Unit =
    Parser.parseFormula(input) should matchPattern:
      case Parsed.Success(`expected`, _) =>
