package com.melvic.lohika

import cats.Show
import cats.implicits.*
import Equivalence.Component
import com.melvic.lohika.formula.Formula

final case class Equivalence(lhs: Component, rhs: Component)

object Equivalence extends EquivalenceImplicits:
  type Component = Formula | Cnf

trait EquivalenceImplicits:
  given componentShow(using Emphasis): Show[Component] = Show.show:
    case fm: Formula => fm.show
    case cnf: Cnf    => cnf.show

  given equivalenceShow(using Emphasis): Show[Equivalence] = Show.show:
    case Equivalence(lhs, rhs) => show"$lhs = $rhs"
