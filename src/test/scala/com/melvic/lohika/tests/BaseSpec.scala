package com.melvic.lohika.tests

import com.melvic.lohika.Formula.*
import com.melvic.lohika.{Cnf, Formula, Parser}
import fastparse.*
import org.scalatest.Assertions.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.annotation.targetName

class BaseSpec extends AnyFlatSpec with should.Matchers

object BaseSpec:
  def assertFromInputStrings(input1: String, input2: String)(f: (Formula, Formula) => Unit): Unit =
    (Parser.parseFormula(input1), Parser.parseFormula(input2)) match
      case (Parsed.Success(formula1, _), Parsed.Success(formula2, _)) =>
        f(formula1, formula2)
      case _ => assert(false)

  extension (self: String)
    @targetName("assertEqualFormulae")
    def ====(other: String): Unit =
      assertFromInputStrings(self, other): (formula1, formula2) =>
        assert(
          formula1 === formula2,
          s"${prettyPrint(formula1)} is not equal to ${prettyPrint(formula2)}"
        )

    @targetName("assertEqualToCnf")
    def ====>(other: String): Unit =
      assertFromInputStrings(self, other): (formula1, formula2) =>
        val cnf = Cnf.fromFormula(formula1)
        assert(
          cnf === formula2,
          s"${prettyPrint(formula1)} has the expected CNF: $cnf. Got: ${prettyPrint(formula2)}"
        )
