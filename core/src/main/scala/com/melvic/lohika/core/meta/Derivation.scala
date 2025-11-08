package com.melvic.lohika.core.meta

import cats.Show
import cats.implicits.{showInterpolator, toShow}
import com.melvic.lohika.core.Formatter
import com.melvic.lohika.core.formula.{Cnf, Formula}
import com.melvic.lohika.core.meta.Derivation.Component

final case class Derivation(from: Component, to: Component)

object Derivation:
  type Component = Formula | Cnf

  given componentShow(using Formatter): Show[Component] = Show.show:
    case fm: Formula => fm.show
    case cnf: Cnf    => cnf.show

  given derivationShow(using Formatter): Show[Derivation] = Show.show:
    case Derivation(from, to) => show"Since $from, it follows that $to"
