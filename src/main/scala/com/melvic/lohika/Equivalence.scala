package com.melvic.lohika

import cats.Show
import com.melvic.lohika.formula.Formula
import cats.implicits.*
import com.melvic.lohika.Equivalence.Component

final case class Equivalence(lhs: Component, rhs: Component)

object Equivalence extends EquivalenceImplicits:
  type Component = Formula | Cnf

trait EquivalenceImplicits:
  given componentShow: Show[Component] = Show.show:
    case fm: Formula => fm.show
    case cnf: Cnf    => cnf.show

  given equivalenceShow: Show[Equivalence] = Show.show:
    case Equivalence(lhs, rhs) => show"$lhs = $rhs"
