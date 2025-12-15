package com.melvic.lohika.controllers

import cats.implicits.*
import com.melvic.lohika.controllers.Eval.Result
import com.melvic.lohika.controllers.symbols.MathJax
import com.melvic.lohika.core.Formatter.{link, sentence}
import com.melvic.lohika.core.Links
import com.melvic.lohika.core.meta.Entailment
import com.melvic.lohika.core.meta.Entailment.{Derived, Direct}
import com.melvic.lohika.core.prover.Prover.given
import com.melvic.lohika.core.prover.{Proof, Prover}

trait Eval:
  def run(rawEntailment: String): Result

object Eval:
  type Result = Either[String, (String, String)]

  def live: Eval = rawEntailment =>
    def handleDirect(entailment: Direct, proof: Proof): Result =
      val entailmentElem = MathJax.applyToText(entailment.show)
      val steps = Proof.toProse(proof)
      val proofMethodStmt =
        s"We use ${"proof by contradiction".link(Links.ProofByContradiction)}".sentence
      val solution = MathJax.applyToText(proofMethodStmt + " " + steps.mkString(" "))
      Right(entailmentElem, solution)

    Prover.prove(rawEntailment) match
      case Left(error)                      => Left(error)
      case Right(entailment: Direct, proof) => handleDirect(entailment, proof)
      case Right(entailment: Derived, proof) =>
        handleDirect(Entailment.unfold(entailment), proof)
