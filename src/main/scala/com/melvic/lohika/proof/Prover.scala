package com.melvic.lohika.proof

import cats.data.Writer
import com.melvic.lohika.Problem
import com.melvic.lohika.formula.Formula

trait Prover[F[_]]:
  def convertToCnfs(formulae: List[Formula]): F[(List[Formula], List[Cnf])]

  def updateClauses(clauses: Clauses, newClauses: Clauses): F[Clauses]

  def negateProposition(proposition: Formula): F[Formula]

object Prover:
  def apply[F[_]](using prover: Prover[F]): Prover[F] = prover
