package com.melvic.lohika.formula

import cats.Endo
import Formula.*

object Converter:
  type ConvertWithFrom[F <: Formula] = Endo[Formula] => F => Formula

  def eliminateBiconditionals: Endo[Formula] =
    case Iff(p, q, Nil) => eliminateBiconditionals((p ==> q) & (q ==> p))
    case Iff(p, q, rs) =>
      val iffs = rs.foldLeft(List(p <==> q)):
        case (iffs @ (Iff(_, q, _) :: _), r) => (q <==> r) :: iffs
      eliminateBiconditionals(And.fromList(iffs.reverse))
    case imply: Imply             => fromImplyWith(eliminateBiconditionals)(imply)
    case or: Or                   => fromOrWith(eliminateBiconditionals)(or)
    case and: And                 => fromAndWith(eliminateBiconditionals)(and)
    case not: Not                 => fromNotWith(eliminateBiconditionals)(not)
    case forall: Forall           => fromForallWith(eliminateBiconditionals)(forall)
    case thereExists: ThereExists => fromThereExistsWith(eliminateBiconditionals)(thereExists)
    case fm                       => fm

  def eliminateImplications: Endo[Formula] =
    case Imply(p, q)              => eliminateImplications(!p | q)
    case or: Or                   => fromOrWith(eliminateImplications)(or)
    case and: And                 => fromAndWith(eliminateImplications)(and)
    case not: Not                 => fromNotWith(eliminateImplications)(not)
    case forall: Forall           => fromForallWith(eliminateImplications)(forall)
    case thereExists: ThereExists => fromThereExistsWith(eliminateImplications)(thereExists)
    case fm                       => fm

  def distributeOrOverAnds: Endo[Formula] =
    case Or(p, And(ap, aq, ars), Nil) =>
      fromAndWith(distributeOrOverAnds)(And(p | ap, p | aq, ars.map(p | _)))
    case Or(And(ap, aq, ars), q, Nil) =>
      fromAndWith(distributeOrOverAnds)(And(ap | q, aq | q, ars.map(_ | q)))
    case Or(p, and: And, r :: rs) => distributeOrOverAnds(Or(distributeOrOverAnds(p | and), r, rs))
    case Or(and: And, q, r :: rs) => distributeOrOverAnds(Or(distributeOrOverAnds(and | q), r, rs))
    case Or(p, q, (and: And) :: rs) =>
      distributeOrOverAnds(Or(p, distributeOrOverAnds(q | and), rs))
    case Or(p, q, r :: rs) => distributeOrOverAnds(p | distributeOrOverAnds(Or(q, r, rs)))
    case and: And          => fromAndWith(distributeOrOverAnds)(and)
    case not: Not          => fromNotWith(distributeOrOverAnds)(not)
    case fm                => fm

  def flattenConjunctions: Endo[Formula] =
    case and: And =>
      val flattened = and.components.map(flattenOrsAndAnds)
      flattened.tail.foldLeft(flattened.head):
        case (And(p, q, rs), and: And) => And(p, q, rs ++ and.components)
        case (And(p, q, rs), fm)       => And(p, q, rs ++ List(fm))
        case (fm, And(p, q, rs))       => And(fm, p, q :: rs)
        case (fm1, fm2)                => fm1 & fm2
    case or: Or => fromOrWith(flattenConjunctions)(or)
    case fm     => fm

  def flattenDisjunctions: Endo[Formula] =
    case or: Or =>
      val flattened = or.components.map(flattenOrsAndAnds)
      flattened.tail.foldLeft(flattened.head):
        case (Or(p, q, rs), or: Or) => Or(p, q, rs ++ or.components)
        case (Or(p, q, rs), fm)     => Or(p, q, rs ++ List(fm))
        case (fm, Or(p, q, rs))     => Or(fm, p, q :: rs)
        case (fm1, fm2)             => fm1 | fm2
    case and: And => fromAndWith(flattenDisjunctions)(and)
    case fm       => fm

  def flattenOrsAndAnds: Endo[Formula] =
    case and: And => flattenConjunctions(and)
    case or: Or   => flattenDisjunctions(or)
    case fm       => fm

  def fromImplyWith: ConvertWithFrom[Imply] = f =>
    case Imply(p, q) => f(p) ==> f(q)

  def fromOrWith: ConvertWithFrom[Or] = f =>
    case Or(p, q, rs) => Or(f(p), f(q), rs.map(f))

  def fromAndWith: ConvertWithFrom[And] = f =>
    case And(p, q, rs) => And(f(p), f(q), rs.map(f))

  def fromNotWith: ConvertWithFrom[Not] = f =>
    case Not(p) => Not(f(p))

  def fromForallWith: ConvertWithFrom[Forall] = f =>
    case Forall(boundVars, matrix) => Forall(boundVars, f(matrix))

  def fromThereExistsWith: ConvertWithFrom[ThereExists] = f =>
    case ThereExists(boundVars, matrix) => ThereExists(boundVars, f(matrix))
