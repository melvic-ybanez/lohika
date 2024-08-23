package com.melvic.lohika.tests.proofs

import com.melvic.lohika.{Cnf, Formula}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import com.melvic.lohika.Formula.*

class CnfSpec extends AnyFlatSpec with should.Matchers:
  "Disjunction" should "be flattened" in:
    Cnf.convertFormula(("A" | "B") | ("C" | ("D" | "E"))) should be(Or.of("A", "B", "C", "D", "E"))

  it should "recursively convert its components to CNFs" in:
    Cnf.convertFormula(("a" ==> "b") | "c" | Not(Not("d"))) should be(Or.of(!"a", "b", "c", "d"))
    Cnf.convertFormula("a" | (!("b" ==> "a") ==> "c") | "c") should be(
      Or.of("a", !"b", "a", "c", "c")
    )

  it should "distribute over conjunctions" in:
    Cnf.convertFormula(("a" & "b") | "c") should be(("a" | "c") & ("b" | "c"))
    Cnf.convertFormula("a" | ("b" & "c")) should be(("b" | "a") & ("c" | "a"))
    Cnf.convertFormula(Or.of("a" & "b", "c", "d")) should be(
      Or.of("a", "c", "d") & Or.of("b", "c", "d")
    )
    Cnf.convertFormula(Or.of("a", "b", "c", "d" & "e")) should be(
      Or.of("d", "a", "b", "c") & Or.of("e", "a", "b", "c")
    )

  "Conjunction" should "be flattend" in:
    Cnf.convertFormula(("A" & "B") & ("C" & ("D" & "E"))) should be(And.of("A", "B", "C", "D", "E"))

  it should "recursively convert its components to CNFs" in:
    Cnf.convertFormula(("a" ==> "b") & "c" & Not(Not("d"))) should be(And.of(!"a" | "b", "c", "d"))
    Cnf.convertFormula("a" & (!("b" ==> "a") ==> "c") & "c") should be(
      And.of("a", Or.of(!"b", "a", "c"), "c")
    )

  "p => q" should "become !p | q" in:
    Cnf.convertFormula("p" ==> "q") should be(!"p" | "q")
    Cnf.convertFormula(("a" ==> "b") ==> ("c" ==> "d")) should be(!(!"a" | "b") | (!"c" | "d"))

  "p <=> q" should "become (p => q) & (q => p) and further converted to CNF" in:
    Cnf.convertFormula("p" <==> "q") should be((!"p" | "q") & (!"q" | "p"))

  "!(p & q)" should "become !p | !q" in:
    Cnf.convertFormula(!("p" & "q")) should be(!"p" | !"q")
    Cnf.convertFormula(!(("p" ==> "q") & "r")) should be(("p" | !"r") & (!"q" | !"r"))

  "!(p | q)" should "become !p & q" in:
    Cnf.convertFormula(!("p" | "q")) should be(!"p" & !"q")
    Cnf.convertFormula(!(("p" ==> "q") | "r")) should be(And.of("p", !"q", !"r"))

  "!(p => q)" should "become p & !q" in:
    Cnf.convertFormula(!("p" ==> "q")) should be("p" & !"q")

  "Double negation" should "cancel out" in:
    Cnf.convertFormula(Not(Not("p"))) should be(Var("p"))
    Cnf.convertFormula(Not(Not(Not("p")))) should be(!"p")
    Cnf.convertFormula(Not(Not(Not(Not("p"))))) should be(Var("p"))
