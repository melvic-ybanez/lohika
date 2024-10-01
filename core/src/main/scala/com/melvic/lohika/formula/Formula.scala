package com.melvic.lohika.formula

import cats.Endo
import com.melvic.lohika.formula.Formula.*
import com.melvic.lohika.formula.Formula.Quantified.BoundVars

import scala.annotation.targetName
import scala.util.chaining.*

type Formula = Or | And | Imply | Iff | Var | Not | True.type | False.type | Quantified | Predicate

object Formula extends FormulaGivens:
  final case class Var(name: String)
  final case class Or(p: Formula, q: Formula, rs: List[Formula]) extends FList
  final case class And(p: Formula, q: Formula, rs: List[Formula]) extends FList
  final case class Imply(p: Formula, q: Formula)
  final case class Iff(p: Formula, q: Formula, rs: List[Formula]) extends FList
  final case class Not(p: Formula)
  case object True
  case object False
  final case class Forall(variables: BoundVars, matrix: Formula) extends Quantified
  final case class ThereExists(variables: BoundVars, matrix: Formula) extends Quantified
  final case class Predicate(name: String, args: List[Var])

  type Property = Formula => Boolean

  sealed trait FList:
    def p: Formula

    def q: Formula

    def rs: List[Formula]

    def components: List[Formula] = p :: q :: rs

  trait Quantified:
    def variables: BoundVars

    def matrix: Formula

  def isInCnf: Property =
    case fm if isLiteral(fm) => true
    case and: And            => and.components.forall(and => isInCnf(and) && !isAnd(and))
    case or: Or              => or.components.forall(isLiteral)
    case _                   => false

  def isLiteral: Property =
    case v: Var               => true
    case Not(Var(_))          => true
    case fm if isConstant(fm) => true
    case _                    => false

  def isConstant: Property =
    case True | False => true
    case _            => false

  def isAnd: Property =
    case _: And => true
    case _      => false

  def isOr: Property =
    case _: Or => true
    case _     => false

  def isClause: Property = fm => isOr(fm) || isLiteral(fm)

  // TODO: See if we can remove this and replace with the one from `Cnf`
  def flatten[A <: FList](make: (Formula, Formula, List[Formula]) => Formula)(
      filter: PartialFunction[Formula, A]
  ): Endo[Formula] =
    def toList: Formula => List[Formula] =
      case fm if filter.isDefinedAt(fm) =>
        val chain = filter(fm)
        toList(chain.p) ++ toList(chain.q) ++ chain.rs.flatMap(toList)
      case fm => List(fm)

    {
      case fm if filter.isDefinedAt(fm) =>
        toList(fm).pipe:
          case h :: t :: rest => make(h, t, rest)
      case fm => fm
    }

  object Quantified:
    type BoundVars = (Var, List[Var])
    type Make[Q <: Quantified] = (BoundVars, Formula) => Q

    type Quantifier = Universal.type | Existential.type
    case object Universal
    case object Existential

    def unapply(quantified: Quantified): Option[(Quantifier, BoundVars, Formula)] =
      quantified match
        case Forall(boundVars, matrix)      => Some(Universal, boundVars, matrix)
        case ThereExists(boundVars, matrix) => Some(Existential, boundVars, matrix)

    def apply(quantifier: Quantifier, boundVars: BoundVars, matrix: Formula): Quantified =
      quantifier match
        case Universal   => Forall(boundVars, matrix)
        case Existential => ThereExists(boundVars, matrix)

    def quantified[Q <: Quantified](varName: String, rest: String*)(
        make: Make[Q]
    ): Formula => Q =
      make((Var(varName), rest.map(Var.apply).toList), _)

    def forall(varName: String, rest: String*): Formula => Forall =
      quantified(varName, rest*)(Forall.apply)

    def thereExists(varName: String, rest: String*): Formula => ThereExists =
      quantified(varName, rest*)(ThereExists.apply)

  object Or:
    def of(p: Formula, q: Formula, rs: Formula*): Or =
      Or(p, q, rs.toList)

    def fromList: List[Formula] => Formula =
      case Nil          => False
      case p :: Nil     => p
      case p :: q :: rs => Or(p, q, rs)

    def flatten: Endo[Formula] =
      Formula.flatten(Or.apply) { case or: Or => or }

  object And:
    def of(p: Formula, q: Formula, rs: Formula*): And =
      And(p, q, rs.toList)

    def fromList: List[Formula] => Formula =
      case Nil          => True
      case p :: Nil     => p
      case p :: q :: rs => And(p, q, rs)

    def flatten: Endo[Formula] =
      Formula.flatten(And.apply) { case and: And => and }

  object Iff:
    def of(p: Formula, q: Formula, rs: Formula*): Iff =
      Iff(p, q, rs.toList)

    def fromList: List[Formula] => Option[Formula] =
      case p :: Nil     => Some(p)
      case p :: q :: rs => Some(Iff(p, q, rs))
      case _            => None

    def flatten: Endo[Formula] =
      Formula.flatten(Iff.apply) { case iff: Iff => iff }

  object Imply:
    def of(p: Formula, qs: Formula*): Imply =
      Imply.fromList(p :: qs.toList)

    def fromList(components: List[Formula]): Imply =
      (components.reduceRight(Imply.apply): @unchecked) match
        case imply: Imply => imply

  extension (self: Formula)
    @targetName("and")
    def &(other: Formula): And = And.of(self, other)

    @targetName("or")
    def |(other: Formula): Or = Or.of(self, other)

    @targetName("implies")
    def ==>(other: Formula): Imply = Imply(self, other)

    @targetName("ifAndOnlyIf")
    def <==>(other: Formula): Iff = Iff.of(self, other)

    @targetName("not")
    def unary_! : Not = Not(self)

  extension (name: String)
    def of(args: String*): Predicate =
      Predicate(name, args.toList.map(Var.apply))
