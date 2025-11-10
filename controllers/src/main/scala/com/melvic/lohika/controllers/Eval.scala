package com.melvic.lohika.controllers

import cats.implicits.*
import com.melvic.lohika.controllers.Eval.Result
import com.melvic.lohika.controllers.symbols.MathJax
import com.melvic.lohika.core.meta.Entailment.{Derived, Direct}
import com.melvic.lohika.core.meta.Entailment
import com.melvic.lohika.core.prover.interpreters.LiveProver.Steps.Steps
import com.melvic.lohika.core.prover.interpreters.LiveProver.{Steps, given}
import com.melvic.lohika.core.prover.programs.ProverProgram

trait Eval:
  def run(rawEntailment: String): Result

object Eval:
  type Result = Either[String, (String, String)]

  def live: Eval = rawEntailment =>
    def handleDirect(entailment: Direct, steps: List[String]): Result =
      val entailmentElem = MathJax.applyToText(entailment.show)
      val solution = MathJax.applyToText(steps.mkString(" "))
      Right(entailmentElem, solution)

    ProverProgram.prove[Steps](rawEntailment).run match
      case Left(error) => Left(error)
      case Right(steps, (entailment: Direct, _)) => handleDirect(entailment, steps)
      case Right(steps, (entailment: Derived, _)) =>
        handleDirect(Entailment.unfold(entailment), steps)
