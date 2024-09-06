package com.melvic.lohika.proof

import cats.data.Writer
import com.melvic.lohika.Problem
import com.melvic.lohika.formula.Formula
import cats.*
import cats.implicits.*

trait Prover[F[_]]:
  def splitAllIntoClauses(cnfs: List[Cnf]): F[Clauses]

  def splitIntoClauses(cnf: Cnf): F[Clauses] = 
    splitAllIntoClauses(cnf :: Nil)

  def convertAllToCnfs(formulae: List[Formula]): F[List[Cnf]]

  def convertToCnf(formula: Formula)(implicit functor: Functor[F]): F[Cnf] =
    convertAllToCnfs(formula :: Nil).map(_.head)

  def updateClauses(clauses: Clauses, newClauses: Clauses): F[Clauses]

  def negateProposition(proposition: Formula): F[Formula]

  def applyResolution(clauses: Clauses): F[Clauses]

object Prover:
  def apply[F[_]](using prover: Prover[F]): Prover[F] = prover
