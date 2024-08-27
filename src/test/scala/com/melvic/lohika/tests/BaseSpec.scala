package com.melvic.lohika.tests

import com.melvic.lohika.Formula.*
import com.melvic.lohika.{Cnf, Formula, Parser}
import fastparse.*
import org.scalactic.Prettifier
import org.scalatest.Assertions.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.annotation.targetName

class BaseSpec extends AnyFlatSpec with should.Matchers:
  given prettifier: Prettifier with
    def apply(obj: Any): String =
      obj match
        case fm: Formula => prettyPrint(fm)
        case Parsed.Success(fm: Formula, index) =>
          Parsed.Success(prettyPrint(fm), index).toString()
        case _ => obj.toString

object BaseSpec:
  def assertFromInputStrings(input1: String, input2: String)(f: (Formula, Formula) => Unit): Unit =
    (Parser.parseFormula(input1), Parser.parseFormula(input2)) match
      case (Parsed.Success(formula1, _), Parsed.Success(formula2, _)) =>
        f(formula1, formula2)
      case (Parsed.Failure(label, _, _), _) =>
        assert(false, s"Unable to parse $input1. Details: $label")
      case (_, Parsed.Failure(label, _, _)) =>
        assert(false, s"Unable to parse $input2. Details: $label")

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
      assertFromInputStrings(self, other): (formula1, expectedCnf) =>
        val cnf = Cnf.fromFormula(formula1)
        assert(
          cnf === expectedCnf,
          s"${prettyPrint(formula1)} has the expected CNF: ${prettyPrint(expectedCnf)}. " +
            s"Got: ${prettyPrint(cnf)}"
        )
