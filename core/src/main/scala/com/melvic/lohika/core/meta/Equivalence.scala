package com.melvic.lohika.core.meta

import cats.Show
import cats.implicits.*
import com.melvic.lohika.core.Formatter
import com.melvic.lohika.core.Formatter
import com.melvic.lohika.core.formula.{Cnf, Formula}
import Equivalence.Component

final case class Equivalence(lhs: Component, rhs: Component)

object Equivalence extends EquivalenceGivens:
  type Component = Formula | Cnf

trait EquivalenceGivens:
  given componentShow(using Formatter): Show[Component] = Show.show:
    case fm: Formula => fm.show
    case cnf: Cnf    => cnf.show

  given equivalenceShow(using Formatter): Show[Equivalence] = Show.show:
    case Equivalence(lhs, rhs) => show"$lhs = $rhs"
