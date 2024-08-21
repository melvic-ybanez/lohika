package com.melvic.lohika.tests.proofs

import com.melvic.lohika.Cnf
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import com.melvic.lohika.Formula._

class CnfSpec extends AnyFlatSpec with should.Matchers:
  "Disjunction" should "flatten" in:
    Cnf.convertFormula(("A" | "B") | ("C" | ("D" | "E"))) should be(Or.of("A", "B", "C", "D", "E"))
    Cnf.convertFormula(("a" ==> "b") | "c" | Not(Not("d"))) should be(Or.of(!"a", "b", "c", "d"))

  "Conjunction" should "flatten" in:
    Cnf.convertFormula(("A" & "B") & ("C" & ("D" & "E"))) should be(And.of("A", "B", "C", "D", "E"))
    Cnf.convertFormula(("a" ==> "b") & "c" & Not(Not("d"))) should be(And.of(!"a" | "b", "c", "d"))

  "p => q" should "become !p | q" in:
    Cnf.convertFormula("p" ==> "q") should be(!"p" | "q")
    Cnf.convertFormula(("a" ==> "b") ==> ("c" ==> "d")) should be(!(!"a" | "b") | (!"c" | "d"))

  "p <=> q" should "become (p => q) & (q => p) and further converted to CNF" in:
    Cnf.convertFormula("p" <==> "q") should be((!"p" | "q") & (!"q" | "p"))
  
  "!(p & q)" should "become !p | !q" in:
    Cnf.convertFormula(!("p" & "q")) should be(!"p" | !"q")
    Cnf.convertFormula(!(("p" ==> "q") & "r")) should be(("p" & !"q") | !"r")

  "!(p | q)" should "become !p & q" in:
    Cnf.convertFormula(!("p" | "q")) should be(!"p" & !"q")
    Cnf.convertFormula(!(("p" ==> "q") | "r")) should be(And.of("p", !"q", !"r"))

  "Double negation" should "cancel out" in:
    Cnf.convertFormula(Not(Not("p"))) should be(Var("p"))
    Cnf.convertFormula(Not(Not(Not("p")))) should be(!"p")
    Cnf.convertFormula(Not(Not(Not(Not("p"))))) should be(Var("p"))
