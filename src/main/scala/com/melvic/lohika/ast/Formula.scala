package com.melvic.lohika.ast

sealed trait Formula

object Formula:
  final case class Var(name: String) extends Formula
  final case class Or(p: Formula, q: Formula) extends Formula
  final case class And(p: Formula, q: Formula) extends Formula
  final case class Imply(p: Formula, q: Formula) extends Formula
  final case class Iff(p: Formula, q: Formula) extends Formula
  final case class Not(p: Formula) extends Formula
