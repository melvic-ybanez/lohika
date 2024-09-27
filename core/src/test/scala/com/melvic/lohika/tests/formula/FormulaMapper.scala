package com.melvic.lohika.tests.formula

import cats.*
import cats.implicits.*
import com.melvic.lohika.formula.Formula
import com.melvic.lohika.tests.BaseSpec.assertFromInputStrings
import FormulaMapper.FormulaMap
import com.melvic.lohika.tests.Givens

import scala.annotation.targetName

trait FormulaMapper:
  givens: Givens =>

  given formulaMap: FormulaMap

  extension (self: String)
    @targetName("assertEqualToCnf")
    def ====>(other: String): Unit =
      assertFromInputStrings(self, other): (formula1, expectedCnf) =>
        val cnf = summon[FormulaMap].transform(formula1)
        assert(
          cnf == expectedCnf,
          show"\n$formula1 has the expected CNF: $expectedCnf. Got: $cnf"
        )

object FormulaMapper:
  final case class FormulaMap(transform: Endo[Formula])
