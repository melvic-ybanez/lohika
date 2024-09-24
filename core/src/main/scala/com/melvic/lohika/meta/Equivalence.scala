package com.melvic.lohika.meta

import cats.Show
import cats.implicits.*
import com.melvic.lohika.formula.Formula
import com.melvic.lohika.meta.Equivalence.Component
import com.melvic.lohika.{Cnf, Formatter}

final case class Equivalence(lhs: Component, rhs: Component)

object Equivalence extends EquivalenceGivens:
  type Component = Formula | Cnf

trait EquivalenceGivens:
  given componentShow(using Formatter): Show[Component] = Show.show:
    case fm: Formula => fm.show
    case cnf: Cnf    => cnf.show

  given equivalenceShow(using Formatter): Show[Equivalence] = Show.show:
    case Equivalence(lhs, rhs) => show"$lhs = $rhs"
