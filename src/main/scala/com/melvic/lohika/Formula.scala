package com.melvic.lohika

sealed trait Formula

object Formula:
  final case class Var(name: String) extends Formula
  final case class Or(p: Formula, q: Formula) extends Formula
  final case class And(p: Formula, q: Formula) extends Formula
  final case class Imply(p: Formula, q: Formula) extends Formula
  final case class Iff(p: Formula, q: Formula) extends Formula
  final case class Not(p: Formula) extends Formula
  case object True extends Formula
  case object False extends Formula

  object Or:
    def fromSeq(components: Seq[Formula]): Or =
      components.tail.foldLeft(components.head)(Or.apply) match
        case or: Or => or
        case _      => Or(False, False)

  object And:
    def fromSeq(components: Seq[Formula]): And =
      components.tail.foldLeft(components.head)(And.apply) match
        case and: And => and
        case _        => And(False, False)
