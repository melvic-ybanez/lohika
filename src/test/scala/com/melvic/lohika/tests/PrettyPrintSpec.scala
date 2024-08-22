package com.melvic.lohika.tests

import com.melvic.lohika.Formula
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import com.melvic.lohika.Formula.*

class PrettyPrintSpec extends AnyFlatSpec with should.Matchers:
  "A variable" should "print its name" in:
    Formula.prettyPrint("A") should be("A")

  "Disjunctions" should "be separated by |s" in:
    Formula.prettyPrint(Or.of("A", "B", "C")) should be("A | B | C")

  it should "wrapped components with lower or equal precedence levels in parens" in:
    Formula.prettyPrint(Or.of("A", "B" | "C", "D")) should be("A | B | C | D")
    Formula.prettyPrint(Or.of("A", "B" ==> "C", "D", "E" <==> "F")) should be(
      "A | (B => C) | D | (E <=> F)"
    )

  it should "not wrapped components with higher precedence levels in parens" in:
    Formula.prettyPrint(Or.of("A", "B" & "C", "D")) should be("A | B & C | D")
    Formula.prettyPrint(Or.of("A", !"B", "D", !"E")) should be("A | !B | D | !E")

  "Conjunctions" should "be separated by &s" in:
    Formula.prettyPrint(And.of("A", "B", "C")) should be("A & B & C")

  it should "wrapped components with lower or equal precedence levels in parens" in:
    Formula.prettyPrint(And.of("A", "B" & "C", "D")) should be("A & B & C & D")
    Formula.prettyPrint(And.of("A", "B", "C" | "D")) should be("A & B & (C | D)")
    Formula.prettyPrint(And.of("A", "B" ==> "C", "D", "E" <==> "F")) should be(
      "A & (B => C) & D & (E <=> F)"
    )
    Formula.prettyPrint("A" | "B" ==> "C") should be("A | (B => C)")

  it should "not wrapped components with higher precedence levels in parens" in:
    Formula.prettyPrint(And.of("A", !"B", "D", !"E")) should be("A & !B & D & !E")

  "Implications" should "be separated by =>s" in:
    Formula.prettyPrint("A" ==> "B") should be("A => B")
    Formula.prettyPrint(Imply.of("A", "B", "C")) should be("A => B => C")

  it should "wrapped components with lower or equal precedence levels in parens" in:
    Formula.prettyPrint(Imply.of("A" ==> "B", "C")) should be("(A => B) => C")
    Formula.prettyPrint(Imply.of("A", "B" ==> "C", "D")) should be("A => (B => C) => D")
    Formula.prettyPrint(Imply.of("A", "B" <==> "C", "D", "E" <==> "F")) should be(
      "A => (B <=> C) => D => (E <=> F)"
    )

  it should "not wrapped components with higher precedence levels in parens" in:
    Formula.prettyPrint(("A" | "B") ==> "C") should be("A | B => C")
    Formula.prettyPrint("A" ==> ("B" & "C")) should be("A => B & C")
    Formula.prettyPrint(Imply.of("A", !"B", "D", !"E")) should be("A => !B => D => !E")
