package com.melvic.lohika.tests

import cats.*
import cats.implicits.*
import com.melvic.lohika.formula.Formula
import com.melvic.lohika.tests.BaseSpec.assertFromInputStrings
import com.melvic.lohika.tests.WithFormulaTransformer.FormulaTransformer

import scala.annotation.targetName

trait WithFormulaTransformer:
  givens: Givens =>

  given formulaTransformer: FormulaTransformer

  extension (self: String)
    @targetName("assertEqualToCnf")
    def ====>(other: String): Unit =
      assertFromInputStrings(self, other): (formula1, expectedCnf) =>
        val cnf = summon[FormulaTransformer].transform(formula1)
        assert(
          cnf == expectedCnf,
          show"\n$formula1 has the expected CNF: $expectedCnf. Got: $cnf"
        )

object WithFormulaTransformer:
  final case class FormulaTransformer(transform: Endo[Formula])
