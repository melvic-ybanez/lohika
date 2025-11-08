package com.melvic.lohika.core.prover.programs

import cats.*
import cats.implicits.*
import com.melvic.lohika.core.Formatter.*
import com.melvic.lohika.core.meta.Entailment.Direct
import com.melvic.lohika.core.prover.algebras.Prover.*
import com.melvic.lohika.core.{Formatter, Links}
import com.melvic.lohika.core.formula.{Clauses, Formula}
import com.melvic.lohika.core.meta.Entailment
import com.melvic.lohika.core.prover.algebras.Prover

import java.text.NumberFormat.Style

object ProverProgram:
  import com.melvic.lohika.core.Givens.given

  def prove[F[_]: Prover: Monad](
      rawEntailment: String
  )(using Formatter): F[(Entailment, ResolutionResult)] =
    for
      entailment <- Prover[F].parseEntailment(rawEntailment)
      Direct(premises, conclusion) = Entailment.unfold(entailment)
      _ <- Prover[F].write(s"We use ${"proof by contradiction".link(Links.ProofByContradiction)}.")
      negatedConclusion = !Formula.addImpliedForall(conclusion)
      _ <- Prover[F].write(show"Suppose $negatedConclusion")
      formulas = premises ++ List(negatedConclusion)
      cnfs      <- Prover[F].convertAllToCnfs(formulas)
      clauses   <- Prover[F].splitAllIntoClauses(cnfs)
      clauseSet <- Prover[F].updateClauseSet(Clauses.empty, clauses)
      result    <- resolveRecursively(premises, conclusion, clauses)
    yield (entailment, result)

  def resolveRecursively[F[_]: Prover: Monad](
      premises: List[Formula],
      conclusion: Formula,
      clauseSet: Clauses
  )(using Formatter): F[ResolutionResult] =
    for
      resolutionResult <- Prover[F].resolve(clauseSet)
      result <- resolutionResult match
        case Exhaustion =>
          for
            _ <- Prover[F].write(s"Resolution options ${"exhausted".emphasize}.")
            _ <- proofResult(premises, conclusion, false)
          yield Exhaustion
        case contradiction @ Contradiction(clause1, clause2) =>
          for
            _ <- Prover[F].write(
              show"We have both $clause1 and $clause2. This is a ${"contradiction".emphasize}."
            )
            _ <- proofResult(premises, conclusion, true)
          yield contradiction
        case Derived(left, right, clause) =>
          for
            _            <- Prover[F].write(show"Since $left and $right, it follows that $clause")
            newClauseSet <- Prover[F].updateClauseSet(clauseSet, Clauses.one(clause))
            result       <- resolveRecursively(premises, conclusion, newClauseSet)
          yield result
    yield result

  def proofResult[F[_]: Prover](
      premises: List[Formula],
      conclusion: Formula,
      provable: Boolean
  )(using Formatter): F[Unit] =
    val followsString = if provable then "follows" else "does not follow"
    val notString = if provable then "" else " not"
    val therefore = "Therefore, "

    if premises.isEmpty then
      Prover[F].write(
        show"$therefore$conclusion is$notString a ${"tautology".link(Links.Tautology)}."
      )
    else Prover[F].write(show"$therefore$conclusion ${followsString.strong} from $premises")
