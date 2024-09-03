package com.melvic.lohika.proof

import cats.data.Writer
import com.melvic.lohika.formula.Formula
import com.melvic.lohika.proof.Step.{UpdateClauses, ConvertFormulae, ConvertToCnfs, Tell}

type Step = Tell | ConvertToCnfs | ConvertFormulae | UpdateClauses

object Step:
  final case class Tell(value: String)
  final case class ConvertToCnfs(description: String, mapping: List[(Formula, Cnf)])
  final case class ConvertFormulae(description: String, mapping: List[(Formula, Formula)])
  final case class UpdateClauses(description: String, clauses: Clauses)

  extension (step: Step)
    def log: Writer[List[Step], Unit] = Writer.tell(step :: Nil)
