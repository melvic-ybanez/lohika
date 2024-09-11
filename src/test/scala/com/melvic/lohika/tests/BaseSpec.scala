package com.melvic.lohika.tests

import com.melvic.lohika.formula.Formula
import com.melvic.lohika.formula.Formula.*
import com.melvic.lohika.{Clauses, Cnf, Parser}
import fastparse.*
import org.scalactic.Prettifier
import org.scalatest.Assertions.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import cats.*
import cats.implicits.*

import scala.annotation.targetName

class BaseSpec extends AnyFlatSpec with should.Matchers:
  given prettifier: Prettifier with
    def apply(obj: Any): String =
      obj match
        case fm: Formula => fm.show
        case Parsed.Success(fm: Formula, index) =>
          Parsed.Success(fm.show, index).toString()
        case clauses: Clauses => clauses.show
        case _                => obj.toString

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
      // we need to import this locally to shadow `convertToEqualizer` from scala-test
      import cats.syntax.eq._

      assertFromInputStrings(self, other): (formula1, formula2) =>
        assert(
          formula1 === formula2,
          show"formula1 is not equal to $formula2"
        )

    @targetName("assertEqualToCnf")
    def ====>(other: String): Unit =
      assertFromInputStrings(self, other): (formula1, expectedCnf) =>
        val cnf = Cnf.fromFormulaUntyped(formula1)
        assert(
          cnf == expectedCnf,
          s"${formula1.show} has the expected CNF: ${expectedCnf.show}. " +
            s"Got: ${cnf.show}"
        )
