package com.melvic.lohika.tests.parsers

import com.melvic.lohika.Formula.{Or, Var}

class ParserSpec extends BaseSpec:
  "A single alphanumeric string" should "map to a variable" in:
    parseSuccess("A", Var("A"))

  it should "support multiple characters" in :
    parseSuccess("Foo", Var("Foo"))

  "Disjunctions" should "be separated by |" in:
    parseSuccess("A | B", Or(Var("A"), Var("B")))
    parseSuccess("some | where", Or(Var("some"), Var("where")))

  it should "support chaining" in:
    parseSuccess("A|B|C", Or(Or(Var("A"), Var("B")), Var("C")))

  it should "support parenthesized components" in:
    parseSuccess("(A | B) | C", Or(Or(Var("A"), Var("B")), Var("C")))


