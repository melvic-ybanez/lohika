package com.melvic.lohika.tests.parsers

import com.melvic.lohika.Formula.{And, Iff, Imply, Or, Var}

class ParserSpec extends BaseSpec:
  "A single alphanumeric string" should "map to a variable" in:
    parseSuccess("A", Var("A"))

  it should "support multiple characters" in:
    parseSuccess("Foo", Var("Foo"))

  "Disjunctions" should "be separated by |" in:
    parseSuccess("A | B", Or(Var("A"), Var("B")))
    parseSuccess("some | where", Or(Var("some"), Var("where")))

  it should "support chaining" in:
    parseSuccess("A|B|C", Or(Or(Var("A"), Var("B")), Var("C")))

  it should "support parenthesized components" in:
    parseSuccess("(A | B) | C", Or(Or(Var("A"), Var("B")), Var("C")))

  "Conjunctions" should "be separated by &" in:
    parseSuccess("A & B", And(Var("A"), Var("B")))
    parseSuccess("some & where", And(Var("some"), Var("where")))

  it should "support chaining" in:
    parseSuccess("A&B&C", And(And(Var("A"), Var("B")), Var("C")))

  it should "support parenthesized components" in:
    parseSuccess("(A & B) & C", And(And(Var("A"), Var("B")), Var("C")))

  it should "have higher precedence than disjunction" in:
    parseSuccess("A | B & C", Or(Var("A"), And(Var("B"), Var("C"))))

  "Implications" should "be connected by =>" in:
    parseSuccess("A => B", Imply(Var("A"), Var("B")))

  it should "have lower precedence than disjunction" in:
    parseSuccess("A | B => C | D", Imply(Or(Var("A"), Var("B")), Or(Var("C"), Var("D"))))

  "Biconditional" should "be connected by <=>" in:
    parseSuccess("A <=> B", Iff(Var("A"), Var("B")))

  it should "have lower precedence than implication" in:
    parseSuccess("A <=> B => C", Iff(Var("A"), Imply(Var("B"), Var("C"))))
