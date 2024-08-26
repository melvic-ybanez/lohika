package com.melvic.lohika.tests.proofs

import com.melvic.lohika.{Cnf, Formula}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import com.melvic.lohika.Formula.*
import com.melvic.lohika.tests.BaseSpec
import BaseSpec.*

class CnfSpec extends BaseSpec:
  "Disjunction" should "be flattened" in:
    "(A | B) | (C | (D | E))" ====> "A | B | C | D | E"

  it should "recursively convert its components to CNFs" in:
    Cnf.fromFormula(("a" ==> "b") | "c" | Not(Not("d"))) should be(Or.of(!"a", "b", "c", "d"))
    Cnf.fromFormula("a" | (!("b" ==> "a") ==> "c") | "c") should be(
      Or.of("a", !"b", "a", "c", "c")
    )

  it should "distribute over conjunctions" in:
    Cnf.fromFormula(("a" & "b") | "c") should be(("a" | "c") & ("b" | "c"))
    Cnf.fromFormula("a" | ("b" & "c")) should be(("b" | "a") & ("c" | "a"))
    Cnf.fromFormula(Or.of("a" & "b", "c", "d")) should be(
      Or.of("a", "c", "d") & Or.of("b", "c", "d")
    )
    Cnf.fromFormula(Or.of("a", "b", "c", "d" & "e")) should be(
      Or.of("d", "a", "b", "c") & Or.of("e", "a", "b", "c")
    )

  it should "return True if at least one component is True" in:
    Cnf.fromFormula(Or.of(True, False, "p")) should be(True)
    Cnf.fromFormula(Or.of(False | "c", "a", "b" | ("d" | True))) should be(True)

  it should "ignore any False component" in:
    Cnf.fromFormula(Or.of("a", False, "b")) should be("a" | "b")
    Cnf.fromFormula(Or.of(False | "c", "a", "b" | ("d" | False))) should be(
      Or.of("c", "a", "b", "d")
    )

  "Conjunction" should "be flattend" in:
    Cnf.fromFormula(("A" & "B") & ("C" & ("D" & "E"))) should be(And.of("A", "B", "C", "D", "E"))

  it should "recursively convert its components to CNFs" in:
    Cnf.fromFormula(("a" ==> "b") & "c" & Not(Not("d"))) should be(And.of(!"a" | "b", "c", "d"))
    Cnf.fromFormula("a" & (!("b" ==> "a") ==> "c") & "c") should be(
      And.of("a", Or.of(!"b", "a", "c"), "c")
    )

  it should "return False if at least one component is False" in:
    Cnf.fromFormula(And.of(True, False, "p")) should be(False)
    Cnf.fromFormula(And.of(True | "c", "a", "b" & ("d" & False))) should be(False)

  it should "ignore any True component" in:
    Cnf.fromFormula(And.of("a", True, "b")) should be("a" & "b")
    Cnf.fromFormula(And.of(True | "c", "a", "b" | ("d" | True))) should be(Var("a"))

  "p => q" should "become !p | q" in:
    Cnf.fromFormula("p" ==> "q") should be(!"p" | "q")
    Cnf.fromFormula(("a" ==> "b") ==> ("c" ==> "d")) should be(!(!"a" | "b") | (!"c" | "d"))

  "p <=> q" should "become (p => q) & (q => p) and further converted to CNF" in:
    Cnf.fromFormula("p" <==> "q") should be((!"p" | "q") & (!"q" | "p"))

  "p <=> q <=> r" should "be the as (p <=> q) & (q <=> r)" in:
    Cnf.fromFormula(Iff.of("p", "q", "r")) should be(
      And.of(!"p" | "q", !"q" | "p", !"q" | "r", !"r" | "q")
    )

  "!(p & q)" should "become !p | !q" in:
    Cnf.fromFormula(!("p" & "q")) should be(!"p" | !"q")
    Cnf.fromFormula(!(("p" ==> "q") & "r")) should be(("p" | !"r") & (!"q" | !"r"))

  "!(p | q)" should "become !p & q" in:
    Cnf.fromFormula(!("p" | "q")) should be(!"p" & !"q")
    Cnf.fromFormula(!(("p" ==> "q") | "r")) should be(And.of("p", !"q", !"r"))

  "!(p => q)" should "become p & !q" in:
    Cnf.fromFormula(!("p" ==> "q")) should be("p" & !"q")

  "Double negation" should "cancel out" in:
    Cnf.fromFormula(Not(Not("p"))) should be(Var("p"))
    Cnf.fromFormula(Not(Not(Not("p")))) should be(!"p")
    Cnf.fromFormula(Not(Not(Not(Not("p"))))) should be(Var("p"))
