package com.melvic.lohika.tests.parsers

import com.melvic.lohika.Formula._

class ParserSpec extends BaseSpec:
  "True" should "be written as T" in:
    parseSuccess("T", True)
    
  "False" should "be written as F" in:
    parseSuccess("F", False)
    
  "A single alphanumeric string" should "map to a variable" in:
    parseSuccess("A", Var("A"))

  it should "support multiple characters" in:
    parseSuccess("Foo", Var("Foo"))

  "Disjunctions" should "be separated by |" in:
    parseSuccess("A | B", "A" | "B")
    parseSuccess("some | where", "some" | "where")

  it should "support chaining" in:
    parseSuccess("A|B|C", Or.of("A", "B", "C"))

  it should "support parenthesized components" in:
    assertEqualFormulae("(A | B) | C", "A | B | C")

  "Conjunctions" should "be separated by &" in:
    parseSuccess("A & B", "A" & "B")
    parseSuccess("some & where", "some" & "where")

  it should "support chaining" in:
    parseSuccess("A&B&C", And.of("A", "B", "C"))

  it should "support parenthesized components" in:
    assertEqualFormulae("(A & B) & C", "A & B & C")

  it should "have higher precedence than disjunction" in:
    parseSuccess("A | B & C", "A" | ("B" & "C"))

  "Implications" should "be connected by =>" in:
    parseSuccess("A => B", "A" ==> "B")

  it should "be right associative" in:
    parseSuccess("A => B => C", "A" ==> ("B" ==> "C"))

  it should "have lower precedence than disjunction" in:
    parseSuccess("A | B => C | D", ("A" | "B") ==> ("C" | "D"))
    assertEqualFormulae("(A | B) => (C | D)", "A | B => C | D")

  "Biconditional" should "be connected by <=>" in:
    parseSuccess("A <=> B", "A" <==> "B")

  it should "have lower precedence than implication" in:
    parseSuccess("A <=> B => C", "A" <==> ("B" ==> "C"))
